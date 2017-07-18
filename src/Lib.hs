{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}
module Lib ( imgHeight
           , imgWidth
           , numIters
           , raysFromCam
           , blackCanvas
           , mainLoop
           , rZipWith3
           , randomGens
           , rayTrace
           , bounce
           , reflect
           , specular
           ) where

import qualified Data.Array.Repa       as R -- for Repa
import qualified Data.Array.Repa.Shape as S
import qualified Data.Vector           as V
import Util
import Object
import Triple
import Control.Monad
import Data.Array.Repa (Array, DIM1, DIM2, U, D, Z (..), (:.)(..), (!), (+^))
import Data.Vector (Vector)
import System.Random
import Data.Maybe
import Data.Angle
import Debug.Trace


-- | Parameters
imgHeight = 300 :: Int --1200
imgWidth  = 300 :: Int --1200
cameraDepth = 300 :: Double
numIters = 20 :: Int
maxBounces = 3 :: Int
-- |


rZipWith3 :: (R.Source r1 a, R.Source r2 b, R.Source r3 c, S.Shape sh) =>
  (a -> b -> c -> d) ->
  Array r1 sh a ->
  Array r2 sh b ->
  Array r3 sh c ->
  Array D sh d
rZipWith3 = ((R.zipWith id .) .) . R.zipWith


randomGens :: Int -> Int -> Array D DIM1 StdGen
randomGens len = R.map mkStdGen . R.fromListUnboxed (Z :. len) . (take len . randoms) . mkStdGen


raysFromCam :: Array D DIM1 Ray
raysFromCam = flatten $ mapIndex camToPixelRay blackCanvas


camToPixelRay :: DIM2 -> Ray
camToPixelRay (Z :. i :. j) = Ray
  { _origin = Triple 0 0 0
  , _vector = normalize $ Triple i' j' cameraDepth }
  where i' = fromIntegral imgHeight / 2 - fromIntegral i
        j' = fromIntegral j - fromIntegral imgWidth / 2


blackCanvas :: Array D DIM2 Vec3
blackCanvas = R.fromFunction (Z :. imgHeight :. imgWidth) $ const black


whiteCanvas :: Array D DIM1 Vec3
whiteCanvas = R.fromFunction (Z :. imgHeight * imgWidth) $ const white


mainLoop :: Int -> Array D DIM1 Vec3 -> (Int, Array D DIM1 Vec3)
mainLoop n canvas = (n + 1, canvas +^ rZipWith3 (rayTrace maxBounces) gens raysFromCam whiteCanvas)
    where gens = randomGens (imgHeight * imgWidth) n

---

rayTrace :: Int -> StdGen -> Ray -> Vec3 -> Vec3
rayTrace 0 _ _ _         = black -- ran out of bounces
rayTrace n gen ray pixel = interactWith $ closestObjectTo ray
  where interactWith Nothing = black -- pixel
        interactWith (Just (object, distance))
          | hitLight  = fmap (_emittance object *) pixel
          | otherwise = rayTrace (n-1) gen' ray' pixel' 
            where hitLight  = _emittance object > 0
                  (_, gen') = random gen :: (Int, StdGen)
                  ray'      = bounce gen ray object distance 
                  pixel'    = pixel * getColor object
                                                  

closestObjectTo :: Ray -> Maybe (Object, Double)
closestObjectTo ray =
  do let pairWithDistance object = fmap (object,) (distanceFrom ray $ _form object)
     let pairs = V.mapMaybe pairWithDistance objects
     let ordering (_, distance1) (_, distance2) = compare distance1 distance2
     guard . not $ V.null pairs
     return $ V.minimumBy ordering pairs 

---

bounce :: StdGen -> Ray -> Object -> Double -> Ray
bounce gen ray object distance = Ray origin vector
  where origin = march ray distance               :: Vec3
        vector = reflect gen object $ _vector ray :: Vec3


reflect :: StdGen -> Object -> Vec3 -> Vec3
reflect gen object vector
  | _reflective object = specular gen 0 vector normal
  | otherwise          = diffuse gen vector normal
  where normal = getNormal $ _form object


specular :: StdGen -> Double -> Vec3 -> Vec3 -> Vec3
specular gen noise vector normal = rotateRel theta phi vector'
  where normal'          = normalize normal
        projection       = fmap (vector `dot` normal' *) normal'
        vector'          = vector + (fmap ((-2) *) projection)

        -- here we offset the angle of reflection by `noise` but ensure that this does not
        -- cause rays to penetrate the surface of the object
        angleWithSurface = (Degrees 90) - (arccosine . abs $ normal' `dot` normalize vector)
        Degrees maxTheta = min angleWithSurface $ Degrees noise 
        [theta, phi] = map Degrees . fst $ randomRangeList gen [(0, maxTheta), (0, 380)]
        

diffuse :: StdGen -> Vec3 -> Vec3 -> Vec3 
diffuse gen _ normal = rotateRel theta phi normal
  where [theta, phi] = map Degrees . fst $ randomRangeList gen [(0, 90), (0, 380)]
