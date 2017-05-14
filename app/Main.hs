{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE FlexibleContexts #-}

module Main ( main
            , RGB8
            ) where

import qualified Codec.Picture.Types          as M
import qualified Data.Array.Repa              as R 
import qualified Data.Array.Repa.Shape        as S
import qualified Data.Array.Repa.Repr.Unboxed as U
import qualified Data.Vector                  as V
import qualified Codec.Picture                as P

import Data.Array.Repa (Array, DIM1, DIM2, U, D, Z (..), (:.)(..), (!))
import System.Environment (getArgs)
import System.FilePath (replaceExtension)
import Object (Object (..), Form (..), distanceFrom, objects, march, getNormal)
import Triple (Triple (..), RGB8, Vec3, normalize, tripleToTuple)
import Lib (flatten, reshape, Ray (..), mapIndex, black,
            white, toSphericalCoords, fromSphericalCoords)
import Data.Vector (Vector)
import Data.Maybe
import Control.Monad
import Control.Monad.Loops
import Debug.Trace

toImage :: (R.Source r RGB8, S.Shape sh) => Array r sh RGB8 -> P.DynamicImage
toImage canvas = P.ImageRGB8 $ P.generateImage fromCoords imgHeight imgWidth
  where fromCoords i j = convert $ canvas' ! (Z :. i :. j)
        convert (Triple r g b) = P.PixelRGB8 r g b
        canvas' = reshape [imgHeight, imgWidth] canvas

-- | Paramters
imgHeight = 200 :: Int --1200
imgWidth  = 200 :: Int --1200
cameraDepth = 10 :: Double

numIters :: Int
numIters = 1
-- |

main :: IO ()
main = do (_, canvas) <- mainLoop
          (P.savePngImage "image.png" . toImage) canvas

mainLoop :: IO (Int, Array D DIM1 RGB8)
mainLoop = iterateUntilM ((== numIters) . fst)
  (\(n, canvas) -> return (n + 1, rayTrace canvas))
  (1, flatten blankCanvas)


blankCanvas :: Array D DIM2 RGB8
blankCanvas = R.fromFunction (Z :. imgHeight :. imgWidth) $ const white

---

rayTrace :: Array D DIM1 RGB8 -> Array D DIM1 RGB8
rayTrace canvas = R.map fst 
  (until allRaysTerminal updateAll $ R.zipWith (,) canvas rays' :: Array D DIM1 (RGB8, Maybe Ray))
                                           
  where allRaysTerminal :: Array D DIM1 (RGB8, Maybe Ray) -> Bool
        allRaysTerminal = fromJust . (R.foldAllP (&&) True . R.map isNothing) . R.map snd

        updateAll :: Array D DIM1 (RGB8, Maybe Ray) -> Array D DIM1 (RGB8, Maybe Ray)
        updateAll = R.map . uncurry $ updateIfNonTerminal

        updateIfNonTerminal :: RGB8 -> Maybe Ray -> (RGB8, Maybe Ray)
        updateIfNonTerminal pixel = maybe (pixel, Nothing) (update pixel)

        rays'   = R.map Just raysFromCam :: Array D DIM1 (Maybe Ray)


raysFromCam :: Array D DIM1 Ray
raysFromCam = flatten $ mapIndex camToPixelRay blankCanvas


camToPixelRay :: DIM2 -> Ray
camToPixelRay (Z :. i :. j) = Ray
  { _origin = pure 0
  , _vector = Triple (fromIntegral i) (fromIntegral j) cameraDepth }

---

update :: RGB8 -> Ray -> (RGB8, Maybe Ray)
update pixel ray = (addColor object' pixel, ray')
  where (mDistance, object) = closestTo ray
        object' = fmap (const $ trace (_name object) object) mDistance
        ray' = do guard . not $ _light object
                  distance <- mDistance
                  bounce ray object distance


addColor :: Maybe Object -> RGB8 -> RGB8
addColor object pixel = maybe black color object
  where color object | _light object = pixel
                     | otherwise     = _color object

---

closestTo :: Ray -> (Maybe Double, Object)
closestTo ray = V.minimumBy (\(d1, _) (d2, _) -> closest d1 d2)
  $ V.zip distances objects :: (Maybe Double, Object) 
  where distances = V.map (distanceFrom ray . _form) objects :: Vector (Maybe Double)


closest :: Maybe Double -> Maybe Double -> Ordering
closest Nothing _ = GT
closest _ Nothing = LT
closest d1 d2     = compare d1 d2

---

bounce :: Ray -> Object -> Double -> Maybe Ray
bounce ray object distance = Just $ Ray { _origin = origin, _vector = vector }
  where origin = march ray distance                             :: Vec3
        vector = getNormal (_form object) `reflect` _vector ray :: Vec3


reflect :: Vec3 -> Vec3 -> Triple Double
normal `reflect` vector = fromSphericalCoords theta phi
  where angles         = map toSphericalCoords [-vector, normal]
        [thetas, phis] = [map fst angles, map snd angles]
        [theta, phi]   = [vectorAngle + 2 * (normalAngle - vectorAngle)
                         | [vectorAngle, normalAngle] <- [thetas, phis]]
