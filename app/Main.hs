{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE FlexibleContexts #-}

module Main ( main
            , RGB8
            ) where

import Control.Lens
import Codec.Picture
import Control.Monad
import Control.Monad.ST
import Data.Array.Repa (Array, DIM1, DIM2, U, D, Z (..), (:.)(..), (!))
import System.Environment (getArgs)
import System.FilePath (replaceExtension)
import qualified Codec.Picture.Types          as M
import qualified Data.Array.Repa              as R -- for Repa
import qualified Data.Array.Repa.Shape        as S
import qualified Data.Array.Repa.Repr.Unboxed as U
import qualified Data.Vector                  as V
import Object
import Data.Maybe
import Triple (Triple (..), RGB8, Vec3, normalize)
import Lib (flatten, Ray (..), Canvas, inf, mapIndex)
import Data.Vector (Vector)

type RGB8' = (Pixel8, Pixel8, Pixel8)



-- | Get image with true color pixels from manifest Repa array.
toImage :: Array U DIM2 RGB8' -> Image PixelRGB8
toImage a = generateImage gen width height
  where Z :. width :. height = R.extent a
        gen x y = let (r,g,b) = a ! (Z :. x :. y)
                  in PixelRGB8 r g b


-- | Paramters

numRays = imgHeight * imgWidth

imgDimensions :: DIM2
imgDimensions = (Z :. imgHeight :. imgWidth)



numIters :: Int
numIters = 100

type ImageArray = Array D DIM2 RGB8'

l = 100000

canvas :: Canvas
canvas = R.fromFunction (Z :. imgHeight :. imgWidth) $ const black

blankCanvas :: Canvas
blankCanvas = R.fromFunction (Z :. imgHeight :. imgWidth) $ const white


main :: IO ()
main = do
  let array = R.fromFunction (Z :. l) $ \(Z :. x) -> Just x :: Maybe Int
  -- let filtered = (R.fromListUnboxed (Z :. l) . catMaybes $ R.toList array) :: Array U DIM1 Int
  filtered <- R.selectP (\n -> isJust $ array ! (Z :. n)) id l 
  print $ filtered ! (Z :. l - 1)
  -- img    <- R.computeUnboxedP repaImg
  -- (savePngImage "image.png" . ImageRGB8 . toImage) img

repaImg :: ImageArray
repaImg = R.fromFunction imgDimensions originalFnc

newRGB :: Int -> Int -> Int -> RGB8'
newRGB x y z = (pixel x, pixel y, pixel z)
  where pixel = fromIntegral . min 0xff

originalFnc :: DIM2 -> RGB8'
originalFnc (Z :. x :. y) =
  let (q, r) = x `quotRem` max 3 y
  in  newRGB q r (q + r + 30)


--- ################################################

-- | Paramters
imgHeight = 2 :: Int --1200
imgWidth  = 4 :: Int --1200
cameraDepth = 10 :: Double

black = pure 0 :: RGB8
white = pure 1 :: RGB8
some_vec = pure 1

infPlane = Object 
  { _color      = black
  , _light      = False
  , _reflective = True
  , _form       = InfinitePlane
             { _normal = some_vec
             , _point  = some_vec }
  }

raysFromCam :: Array D DIM1 Ray
raysFromCam = flatten $ mapIndex camToPixelRay canvas

camToPixelRay :: DIM2 -> Ray
camToPixelRay (Z :. i :. j)  = Ray { _origin   = pure 0
                                   , _vector   = Triple i' j' cameraDepth }
  where (i', j') = (fromIntegral i, fromIntegral j)



objects :: Vector Object
objects = V.fromList [infPlane] 

-- repaImg' = vec3ToImage $ iterate rayTrace blankCanvas !! numIters :: ImageArray

-- rayTrace :: Vec3 -> Vec3
-- rayTrace canvas = canvas
--   where (_, canvas') = until allTerminal bounce (raysFromCam, canvas)
--         allTerminal (nonTerminalRays, _)  = _num nonTerminalRays == 0

getNormal :: Form -> Vec3
getNormal form = _normal form

toSphericalCoords :: Vec3 -> (Double, Double)
toSphericalCoords coord = (theta, phi)
  where [theta, phi]  = map acos [x', z]
        Triple x y z  = normalize coord
        Triple x' _ _ = normalize $ Triple x y 0

fromSphericalCoords :: Floating t => t -> t -> Triple t
fromSphericalCoords theta phi = Triple x y z
  where x = sin phi * cos theta
        y = sin phi * sin theta
        z = cos phi


reflect :: Vec3 -> Vec3 -> Triple Double
normal `reflect` vector = fromSphericalCoords theta phi
  where angles         = map toSphericalCoords [-vector, normal]
        [thetas, phis] = [map fst angles, map snd angles]
        [theta, phi]   = [vectorAngle + 2 * (normalAngle - vectorAngle)
                         | [vectorAngle, normalAngle] <- [thetas, phis]]


closest :: Maybe Double -> Maybe Double -> Ordering
closest d1 d2 = compare d1' d2'
  where [d1', d2'] = map (fromMaybe inf) [d1, d2]


getClosestObject :: Ray -> Maybe Object
getClosestObject ray = fmap (const closestObject) closestDistance
  where distances          = V.map (distanceFrom ray . _form) objects   :: Vector (Maybe Double)
        compareSnd (_, d1) (_, d2) = closest d1 d2
        (closestObject, closestDistance) = V.minimumBy compareSnd $ V.zip objects distances
  
addColor :: Maybe Object -> RGB8 -> RGB8
addColor object pixel = maybe black ((pixel *) . _color) object


bounce ray object distance = Just $ Ray { _origin = origin, _vector = vector }
  where origin = march ray distance                             :: Vec3
        vector = getNormal (_form object) `reflect` _vector ray :: Vec3


closestTo :: Ray -> (Maybe Double, Object)
closestTo ray = V.minimumBy (\(d1, _) (d2, _) -> closest d1 d2) $ V.zip distances objects
                                                             :: (Maybe Double, Object) 
  where distances = V.map (distanceFrom ray . _form) objects :: Vector (Maybe Double)


allTerminal ::
  (R.Source r (Maybe a), Monad m, S.Shape sh) =>
  Array r sh (Maybe a) -> m Bool
allTerminal rays = R.foldAllP (&&) True isTerminal
  where isTerminal = R.map isNothing rays


update :: RGB8 -> Ray -> (RGB8, Maybe Ray)
update pixel ray = (addColor object' pixel, bounce ray object =<< mDistance)
  where (mDistance, object) = closestTo ray
        object' = fmap (const object) mDistance

rayTrace :: Array D DIM1 (RGB8, Maybe Ray)
rayTrace = until allRaysTerminal updateAll $ R.zipWith (,) canvas' rays'
                                           
  where allRaysTerminal :: Array D DIM1 (RGB8, Maybe Ray) -> Bool
        allRaysTerminal = fromJust . allTerminal . R.map snd

        updateIfNonTerminal :: RGB8 -> Maybe Ray -> (RGB8, Maybe Ray)
        updateIfNonTerminal = maybe (black, Nothing) . update

        updateAll :: Array D DIM1 (RGB8, Maybe Ray) -> Array D DIM1 (RGB8, Maybe Ray)
        updateAll = R.map . uncurry $ updateIfNonTerminal

        canvas' = flatten blankCanvas    :: Array D DIM1 RGB8
        rays'   = R.map Just raysFromCam :: Array D DIM1 (Maybe Ray)
