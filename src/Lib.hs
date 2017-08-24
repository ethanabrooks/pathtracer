{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Lib
  ( imgHeight
  , imgWidth
  , numIters
  , raysFromCam
  , mainLoop
  , randomGens
  , rayTrace
  , bounce
  , reflect
  , specular
  ) where

import Control.Monad
import Data.Angle
import Data.Array.Repa
       ((:.)(..), Array, D, DIM1, DIM2, U, Z(..), (!), (+^))
import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.Shape as S
import Data.Maybe
import qualified Data.Vector as V
import Debug.Trace
import Object
import System.Random
import Triple
import Util

-- | Parameters
imgHeight = 300 :: Int --1200

imgWidth = 300 :: Int --1200

cameraDepth = 300 :: Double

numIters = 50 :: Int

maxBounces = 3 :: Int

-- |
randomGens :: Int -> Int -> Array D DIM1 StdGen
randomGens len =
  R.map mkStdGen .
  R.fromListUnboxed (Z :. len) . (take len . randoms) . mkStdGen

raysFromCam :: Array D DIM1 Ray
raysFromCam =
  flatten $ R.fromFunction (Z :. imgHeight :. imgWidth) camToPixelRay

camToPixelRay :: DIM2 -> Ray
camToPixelRay (Z :. i :. j) =
  Ray
  { _origin = Point $ pure 0
  , _vector = Vector $ normalize $ Triple i' j' cameraDepth
  }
  where
    i' = fromIntegral imgHeight / 2 - fromIntegral i
    j' = fromIntegral j - fromIntegral imgWidth / 2

mainLoop :: (Int, Array D DIM1 (Triple Double))
         -> (Int, Array D DIM1 (Triple Double))
mainLoop (seed, canvas) = (seed + 1, canvas +^ newColor)
  where
    newColor = R.zipWith (rayTrace maxBounces Nothing white) gens raysFromCam
    gens = randomGens (imgHeight * imgWidth) seed

---
rayTrace :: Int
         -> Maybe Object
         -> Triple Double
         -> StdGen
         -> Ray
         -> Triple Double
rayTrace 0 _ _ _ _ = black -- ran out of bounces
rayTrace iterations lastStruck pixel gen ray =
  interactWith $ closestObjectTo ray lastStruck
  where
    interactWith :: Maybe (Object, Double) -> Vec3
    interactWith Nothing = black -- pixel
    interactWith (Just (object, distance))
      | hitLight = fmap (_emittance object *) pixel
      | otherwise = rayTrace (iterations - 1) (Just object) pixel' gen' ray'
      where
        hitLight = _emittance object > 0 :: Bool
        (_, gen') = random gen :: (Int, StdGen)
        ray' = bounce gen ray object distance :: Ray
        pixel' = pixel * getColor object :: Triple Double

closestObjectTo :: Ray -> Maybe Object -> Maybe (Object, Double)
closestObjectTo ray lastStruck = do
  guard . not $ V.null pairs
  return $ V.minimumBy distanceOrdering pairs
  where
    pairWithDistance :: Object -> Maybe (Object, Double)
    pairWithDistance object = fmap (object, ) (distanceFrom ray $ _form object)
    objectsWithout :: Object -> V.Vector Object
    objectsWithout lastStruck' = V.filter (lastStruck' /=) objects
    objects' :: V.Vector Object
    objects' = maybe objects objectsWithout $ lastStruck
    pairs :: V.Vector (Object, Double)
    pairs = V.mapMaybe pairWithDistance objects'
    distanceOrdering :: (Object, Double) -> (Object, Double) -> Ordering
    distanceOrdering (_, distance1) (_, distance2) = compare distance1 distance2

---
bounce :: StdGen -> Ray -> Object -> Double -> Ray
bounce gen ray object distance = Ray origin vector
  where
    origin = Point $ march ray distance
    vector = Vector $ reflect gen object $ getVector ray

reflect :: StdGen -> Object -> Triple Double -> Triple Double
reflect gen object vector
  | _reflective object = specular gen 0 vector normal
  | otherwise = diffuse gen vector normal
  where
    normal = getNormal $ _form object

specular :: StdGen -> Double -> Triple Double -> Triple Double -> Triple Double
specular gen noise vector normal =
  rotateRel (Degrees theta) (Degrees phi) vector'
  where
    normal' = normalize normal
    projection = fmap (vector `dot` normal' *) normal'
    vector' = vector + (-2) * projection
        -- here we offset the angle of reflection by `noise` but ensure that this does not
        -- cause rays to penetrate the surface of the object
    angleWithSurface =
      (Degrees 90) - (arccosine . abs $ normal' `dot` normalize vector)
    Degrees maxTheta = min angleWithSurface $ Degrees noise
    ([theta, phi], _) = randomRangeList gen [(0, maxTheta), (0, 380)]

diffuse :: StdGen -> Triple Double -> Triple Double -> Triple Double
diffuse gen _ normal = rotateRel (Degrees theta) (Degrees phi) normal
  where
    ([theta, phi], _) = randomRangeList gen [(0, 90), (0, 380)]
