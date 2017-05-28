{-# LANGUAGE TemplateHaskell #-}

import qualified Test.QuickCheck as T
import Test.QuickCheck (quickCheckAll, (==>), quickCheck)
import Triple
import Object
import Lib
import Util
import Control.Applicative
import Data.Angle
import System.Random
import Debug.Trace

propNorm2 :: Bool
propNorm2 = norm2 (Triple 3 4 5) ~= 7.0710678118654755
  where (~=) = aeqWithinTolerance 1e-6

propNormalize :: Triple Double -> T.Property
propNormalize triple = tSum triple > 10**(-6) ==> norm2 (normalize triple) ~= 1
  where (~=) = aeqWithinTolerance 1e-6

propNormalize2 :: Bool
propNormalize2 = norm2 (normalize $ Triple 0 0 0) == 0

contains (low, lowInclusive, high, highInclusive) x = aboveLow && belowHigh
  where aboveLow  | lowInclusive  = low <= x
                  | otherwise     = low < x
        belowHigh | highInclusive = x <= high
                  | otherwise     = x < high
    

  
sphericalRelations x y z x' y' theta phi =
  x' ~= cosine phi &&
  y' ~= sine phi &&
  (y / x) ~= tangent phi &&
  ((x**2) + (y**2)) ~= ((sine theta)**2) &&
  z ~= cosine theta &&
  (((x**2) + (y**2)) / (z**2)) ~= ((tangent theta)**2)
  where (~=) = aeqWithinTolerance 1e-6

propFromSpherical :: Double -> Double -> T.Property
propFromSpherical theta phi = all ((0, False, 180, True) `contains`) [theta, phi] ==>
                                sphericalRelations x y z x' y' theta' phi'
  where [theta', phi'] = map Degrees [theta, phi]
        Triple x  y  z = fromSphericalCoords theta' phi' :: Vec3
        Triple x' y' _ = normalize $ Triple x y 0

propToSpherical :: Vec3 -> T.Property
propToSpherical vec3 = not (norm2 vec3 ~= 0) ==>
  sphericalRelations x y z x' y' theta phi
  where (~=) = aeqWithinTolerance 1e-6
        Triple x  y  z = normalize vec3
        Triple x' y' _ = normalize $ Triple x y 0
        (theta, phi) = toSphericalCoords vec3

propSpherical :: Double -> Double -> T.Property
propSpherical theta phi =
  (0, False, 180, True) `contains` theta && (0, True, 360, True) `contains` phi ==>
  Degrees theta ~= theta' && Degrees phi ~= phi'
  where (~=) = aeqWithinTolerance 1e-6
        (theta', phi') = toSphericalCoords $ fromSphericalCoords (Degrees theta) (Degrees phi) 

propSpherical' :: Vec3 -> T.Property
propSpherical' vec = not (norm2 vec ~= 0) ==>
  (normalize vec) ^~= vec'
  where (~=) = aeqWithinTolerance 1e-6
        (^~=) = vecAeq 1e-6
        (theta, phi) = toSphericalCoords vec
        vec' = fromSphericalCoords theta phi


propRotateAbs1 :: Double -> Double -> T.Property
propRotateAbs1 theta phi =
  (1, False, 180, True) `contains` theta && (0, True, 360, True) `contains` phi ==>
  (rotateAbs (Triple 0 0 1) theta' phi') ~= (fromSphericalCoords theta' phi')
  where (~=) = vecAeq 1e-6
        [theta', phi'] = map Degrees [theta, phi]

propRotateAbs2 :: Double -> Double -> Vec3 -> T.Property
propRotateAbs2 theta phi vector =
  (1, False, 180, True) `contains` theta && (0, True, 360, True) `contains` phi ==>
  norm2 vector ~= norm2 vector'
  where (~=) = aeqWithinTolerance 1e-6
        [theta', phi'] = map Degrees [theta, phi]
        vector' = rotateAbs vector theta' phi'

propRotateAbs3 :: Double -> Double -> Vec3 -> Vec3 -> T.Property
propRotateAbs3 theta phi vector1 vector2 =
  (1, False, 180, True) `contains` theta && (0, True, 360, True) `contains` phi ==>
  (vector1 `dot` vector2) ~= (vector1' `dot` vector2')
  where (~=) = aeqWithinTolerance 1e-6
        [theta', phi'] = map Degrees [theta, phi]
        [vector1', vector2'] = map (\v -> rotateAbs v theta' phi') [vector1, vector2]

propRotateRel :: Double -> Double -> Vec3 -> T.Property
propRotateRel theta phi vector =
  (1, False, 180, True) `contains` theta && (0, True, 360, True) `contains` phi ==>
  cosine theta' ~= (normalize vector `dot` normalize vector')
  where (~=) = aeqWithinTolerance 1e-6
        [theta', phi'] = map Degrees [theta, phi]
        vector' = rotateRel theta' phi' vector

propSpecular :: Vec3 -> Triple Double -> T.Property
propSpecular vector normal = not (norm2 vector ~= 0 || norm2 normal ~= 0) ==>
  -- angle of incidence equals angle of reflection
  (normalize (-vector) `dot` normalize normal) ~= (normalize vector' `dot` normalize normal)
  -- projection onto surface is unchanged
  && (projOntoSurface vector) ^~= (projOntoSurface vector')
  where (~=)  = aeqWithinTolerance 1e-6
        (^~=) = vecAeq 1e-6
        vector' = specular (mkStdGen 0) 0 vector normal
        normal' = normalize normal
        projOntoSurface v = v - fmap (v `dot` normal' *) normal'


vecAeq :: (Ord a, Num a) => a -> Triple a -> Triple a -> Bool
vecAeq tolerance a b = tAnd $ liftA2 (aeqWithinTolerance tolerance) a b


return []
runTests = $quickCheckAll

main = do runTests
