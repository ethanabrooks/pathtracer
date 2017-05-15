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
import Triple (Triple (..), RGB8, Vec3, normalize, tripleToTuple, tripleToList)
import Lib (flatten, reshape, Ray (..), mapIndex, black,
            white, toSphericalCoords, fromSphericalCoords)
import Data.Vector (Vector)
import Data.Maybe
import Control.Monad
import Control.Monad.Loops
import Debug.Trace

toImage :: (R.Source r RGB8, S.Shape sh) => Array r sh RGB8 -> P.DynamicImage
toImage canvas = P.ImageRGB8 $ P.generateImage fromCoords imgHeight imgWidth
  where fromCoords i j = convert $ canvas' ! (Z :. i :. j) * 255
        convert (Triple r g b) = P.PixelRGB8 r g b
        canvas' = reshape [imgHeight, imgWidth] canvas

-- | Paramters
imgHeight = 150 :: Int --1200
imgWidth  = 150 :: Int --1200
cameraDepth = 10 :: Double

numIters :: Int
numIters = 2
-- |

main :: IO ()
main = do (_, canvas) <- mainLoop
          (P.savePngImage "image.png" . toImage) canvas


mainLoop :: IO (Int, Array D DIM1 RGB8)
mainLoop = iterateUntilM ((== numIters) . fst)
  (\(n, canvas) -> return (n + 1, R.zipWith rayTrace raysFromCam canvas))
  (1, flatten blankCanvas)


raysFromCam :: Array D DIM1 Ray
raysFromCam = flatten $ mapIndex camToPixelRay blankCanvas


camToPixelRay :: DIM2 -> Ray
camToPixelRay (Z :. i :. j) = Ray
  { _origin = pure 0
  , _vector = Triple (fromIntegral i) (fromIntegral j) cameraDepth }


blankCanvas :: Array D DIM2 RGB8
blankCanvas = R.fromFunction (Z :. imgHeight :. imgWidth) $ const white


-- simpleRGB :: Array D DIM1 RGB8
-- simpleRGB = R.fromFunction (Z :. 2) (\(Z :. i) -> let i' = fromIntegral i :: P.Pixel8
--                                                       in Triple i' (i'+1) (i'+2)) 
-- simpleDim2 :: Array U DIM2 Int
-- simpleDim2 = R.computeS $ R.fromFunction (Z :. 2 :. 3) (\(Z :. i :. j) -> fromIntegral $ i + j)


-- rgb8todim2
--   :: R.Source r RGB8 => Array r DIM1 RGB8 -> Array U DIM2 Int
-- rgb8todim2 array = R.computeS $ R.traverse array (\(Z :. i) -> (Z :. i :. 3))
--   (\src (Z :. i :. j) -> fromIntegral $ tripleToList (src (Z :. i)) !! j)

-- dim2torgb8
--   :: R.Source r Int => Array r DIM2 Int -> Array D DIM1 RGB8
-- dim2torgb8 array = R.traverse array (\(Z :. i :. _) -> (Z :. i))
--   (\src (Z :. i) -> let get j = fromIntegral $ src (Z :. i :. j)
--                         in Triple (get 0) (get 1) (get 2))

---

rayTrace :: Ray -> RGB8 -> RGB8
rayTrace ray pixel = snd $ until (isNothing . fst) update (Just ray, pixel)

---

update :: (Maybe Ray, RGB8) -> (Maybe Ray, RGB8)
update (Nothing, pixel)       = (Nothing, pixel)
update ((Just ray), pixel)    =
  case closestTo ray of
    Nothing                  -> (Nothing, black)
    Just (object, distance)  -> stopAtLight object
      where stopAtLight object
              | _light object = (Nothing, pixel)
              | otherwise     = (bounce ray object distance, pixel * _color object)


closestTo :: Ray -> Maybe (Object, Double)
closestTo ray = V.minimumBy closest $ V.map distanceTo objects
  where distanceTo object = fmap ((,) object) (distanceFrom ray $ _form object)


closest :: Maybe (Object, Double) -> Maybe (Object, Double) -> Ordering
closest Nothing _ = GT
closest _ Nothing = LT
closest (Just (_, d1)) (Just (_, d2)) = compare d1 d2

---

bounce :: Ray -> Object -> Double -> Maybe Ray
bounce ray object distance = Just $ Ray { _origin = origin, _vector = vector }
  where origin = march ray distance                             :: Vec3
        vector = object `reflect` _vector ray :: Vec3


reflect :: Object -> Vec3 -> Triple Double
object `reflect` vector
  | _reflective object = uncurry fromSphericalCoords $ specular vAngles nAngles
  | otherwise          = vector
  where [vAngles, nAngles] = map toSphericalCoords [-vector, getNormal $ _form object]
  --       [thetas, phis] = [map fst angles, map snd angles]
  --       [theta, phi]   = [vectorAngle + 2 * (normalAngle - vectorAngle)
  --                        | [vectorAngle, normalAngle] <- [thetas, phis]]

specular (vTheta, vPhi) (nTheta, nPhi) = (theta, phi)
  where [theta, phi] = [vAngle + 2 * (nAngle - vAngle)
                       | (vAngle, nAngle) <- [(vTheta, nTheta), (vPhi, nPhi)]]

  
