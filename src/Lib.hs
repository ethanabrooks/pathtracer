{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE Arrows #-}

module Lib
  ( bounceRay
  , reflectVector
  , specular
  , traces
  ) where

import qualified Codec.Picture as P
import Control.Arrow
import Control.Monad
import Data.Angle (Degrees(..), arccosine)
import Data.Array.Repa
       ((:.)(..), Array, D, DIM3, DIM1, DIM2, U, Z(..), (!), (+^))
import qualified Data.Array.Repa as R
import qualified Data.Vector as V
import Object
       (Object(..), Ray(..), Point(..), Vector(..), getColor, getNormal,
        distanceFrom, objects, march, getVector, Color(..))
import qualified Params
import qualified System.Random as Random
import Triple (Triple(..), Vec3, normalize, dot)
import Util
       (flatten, white, black, rotateRel, randomRangeList,
        fromTripleArray)

blackCanvas :: Array D DIM2 Vec3
blackCanvas = R.fromFunction (Z :. Params.height :. Params.width) $ const black

startingGens :: Array D DIM2 Random.StdGen
startingGens =
  R.map Random.mkStdGen $
  R.fromListUnboxed (Z :. Params.height :. Params.width) .
  take (Params.height * Params.width) $
  Random.randoms (Random.mkStdGen 0)

rayFromCamToPixel :: Random.StdGen -> DIM2 -> Ray
rayFromCamToPixel gen (Z :. i :. j) =
  Ray
  { _origin = Point $ pure 0
  , _vector = Vector $ normalize $ Triple i' j' Params.cameraDepth
  , _gen = gen -- Random.mkStdGen $ uniqueId i j iteration
  , _lastStruck = Nothing
  }
  where
    i' = fromIntegral Params.height / 2 - fromIntegral i
    j' = fromIntegral j - fromIntegral Params.width / 2

traces :: [Array D DIM3 Double]
traces =
  map (fromTripleArray . R.map fst) $
  iterate traceCanvas $ R.zipWith (,) blackCanvas startingGens

traceCanvas :: (Array D DIM2 (Vec3, Random.StdGen))
            -> (Array D DIM2 (Vec3, Random.StdGen))
traceCanvas array =
  R.traverse array id $ \lookup sh ->
    let (color, gen) = lookup sh
        ray = rayFromCamToPixel gen sh
        (newColor, gen') = terminalColor Params.maxBounces white ray
    in (color + newColor, gen')

terminalColor :: Int -> Vec3 -> Ray -> (Vec3, Random.StdGen)
terminalColor 0 _ ray = (black, _gen ray) -- ran out of bounces
terminalColor bouncesLeft pixel ray = interactWith $ closestObjectTo ray
  where
    interactWith :: Maybe (Object, Double) -> (Vec3, Random.StdGen)
    interactWith Nothing = (black, _gen ray) -- pixel
    interactWith (Just (object, distance))
      | hitLight = ((_emittance object *) <$> pixel, _gen ray)
      | otherwise = terminalColor (bouncesLeft - 1) pixel' ray'
      where
        hitLight = _emittance object > 0 :: Bool
        ray' = bounceRay ray object distance :: Ray
        pixel' = pixel * getColor object :: Vec3

closestObjectTo :: Ray -> Maybe (Object, Double)
closestObjectTo ray = do
  guard . not $ V.null pairs -- not all Nothing
  return $ V.minimumBy distanceOrdering pairs
  where
    objectsWithoutLastStruck :: V.Vector Object
    objectsWithoutLastStruck =
      case _lastStruck ray of
        Nothing -> objects
        Just lastStruck -> V.filter (lastStruck /=) objects
    pairs :: V.Vector (Object, Double)
    -- Drop objects with negative and infinite distances
    pairs = V.mapMaybe pairWithDistance objectsWithoutLastStruck
    pairWithDistance :: Object -> Maybe (Object, Double)
    -- Nothing if distance is negative or infinite
    pairWithDistance object = (object, ) <$> (distanceFrom ray $ _form object)
    distanceOrdering :: (Object, Double) -> (Object, Double) -> Ordering
    distanceOrdering (_, distance1) (_, distance2) = compare distance1 distance2

---
bounceRay :: Ray -> Object -> Double -> Ray
bounceRay ray@(Ray {_gen = gen}) object distance =
  Ray (Point origin) (Vector vector) gen' (Just object)
  where
    origin = march ray distance
    (vector, gen') = reflectVector gen object $ getVector ray

reflectVector :: Random.StdGen -> Object -> Vec3 -> (Vec3, Random.StdGen)
reflectVector gen object vector
  | _reflective object = specular gen 40 vector normal
  | otherwise = diffuse gen vector normal
  where
    normal = getNormal $ _form object

specular :: Random.StdGen -> Double -> Vec3 -> Vec3 -> (Vec3, Random.StdGen)
specular gen noise vector normal =
  (rotateRel (Degrees theta) (Degrees phi) vector', gen')
  where
    normal' = normalize normal
    projection = (vector `dot` normal' *) <$> normal'
    vector' = vector + (-2) * projection
        -- here we offset the angle of reflection by `noise` but ensure that this does not
        -- cause rays to penetrate the surface of the object
    angleWithSurface =
      (Degrees 90) - (arccosine . abs $ normal' `dot` normalize vector)
    Degrees maxTheta = min angleWithSurface $ Degrees noise
    ([theta, phi], gen') = randomRangeList gen [(0, maxTheta), (0, 380)]

diffuse :: Random.StdGen -> Vec3 -> Vec3 -> (Vec3, Random.StdGen)
diffuse gen _ normal = (rotateRel (Degrees theta) (Degrees phi) normal, gen')
  where
    ([theta, phi], gen') = randomRangeList gen [(0, 90), (0, 380)]
