{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Lib
  ( imgHeight
  , imgWidth
  , numIters
  , raysFromCam
  , traceCanvas
  , bounceRay
  , reflectVector
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
raysFromCam :: Int -> Array D DIM1 Ray
raysFromCam iteration =
  flatten $
  R.fromFunction (Z :. imgHeight :. imgWidth) (rayFromCamToPixel iteration)

rayFromCamToPixel :: Int -> DIM2 -> Ray
rayFromCamToPixel iteration (Z :. i :. j) =
  Ray
  { _origin = Point $ pure 0
  , _vector = Vector $ normalize $ Triple i' j' cameraDepth
  , _gen = mkStdGen seed
  , _lastStruck = Nothing
  }
  where
    i' = fromIntegral imgHeight / 2 - fromIntegral i
    j' = fromIntegral j - fromIntegral imgWidth / 2
    seed = (iteration * imgHeight * imgWidth) + (i * imgWidth + j)

traceCanvas :: (Int, Array D DIM1 (Triple Double))
            -> (Int, Array D DIM1 (Triple Double))
traceCanvas (iteration, canvas) = (iteration + 1, canvas +^ newColor)
  where
    newColor = R.map (terminalColor maxBounces white) (raysFromCam iteration)

---
terminalColor :: Int -> Triple Double -> Ray -> Triple Double
terminalColor 0 _ _ = black -- ran out of bounces
terminalColor bouncesLeft pixel ray = interactWith $ closestObjectTo ray
  where
    interactWith :: Maybe (Object, Double) -> Vec3
    interactWith Nothing = black -- pixel
    interactWith (Just (object, distance))
      | hitLight = fmap (_emittance object *) pixel
      | otherwise = terminalColor (bouncesLeft - 1) pixel' ray'
      where
        hitLight = _emittance object > 0 :: Bool
        ray' = bounceRay ray object distance :: Ray
        pixel' = pixel * getColor object :: Triple Double

closestObjectTo :: Ray -> Maybe (Object, Double)
closestObjectTo ray = do
  guard . not $ V.null pairs
  return $ V.minimumBy distanceOrdering pairs
  where
    pairWithDistance :: Object -> Maybe (Object, Double)
    pairWithDistance object = fmap (object, ) (distanceFrom ray $ _form object)
    objectsWithout :: Object -> V.Vector Object
    objectsWithout lastStruck' = V.filter (lastStruck' /=) objects
    objects' :: V.Vector Object
    objects' = maybe objects objectsWithout $ _lastStruck ray
    pairs :: V.Vector (Object, Double)
    pairs = V.mapMaybe pairWithDistance objects'
    distanceOrdering :: (Object, Double) -> (Object, Double) -> Ordering
    distanceOrdering (_, distance1) (_, distance2) = compare distance1 distance2

---
bounceRay :: Ray -> Object -> Double -> Ray
bounceRay ray@(Ray {_gen = gen}) object distance =
  Ray origin vector gen' $ Just object
  where
    origin = Point $ march ray distance
    vector = Vector $ reflectVector gen object $ getVector ray
    (_, gen') = random gen :: (Int, StdGen)

reflectVector :: StdGen -> Object -> Triple Double -> Triple Double
reflectVector gen object vector
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
