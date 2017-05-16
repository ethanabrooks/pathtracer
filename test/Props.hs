{-# LANGUAGE TemplateHaskell #-}

import qualified Test.QuickCheck as T
import Test.QuickCheck (quickCheckAll, (==>))
import Triple
import Object
import Lib
import Util
import Control.Applicative
import Data.Angle
import Debug.Trace

(~=) :: (Num a, Ord a, Fractional a) => a -> a -> Bool
a ~= b = (a - b) * (a - b) < 0.001

prop_norm2 :: Bool
prop_norm2 = norm2 (Triple 3 4 5) ~= 7.0710678118654755

prop_normalize :: Triple Double -> T.Property
prop_normalize triple = tSum triple > 10**(-6) ==> norm2 (normalize triple) ~= 1

prop_normalize2 :: Bool
prop_normalize2 = norm2 (normalize $ Triple 0 0 0) == 0

validAngle :: (Num a, Ord a) => a -> Bool
validAngle angle = 10 < angle && angle < 180

spherical_relations x y z x' y' theta phi =
  x' ~= cosine phi &&
  y' ~= sine phi &&
  (y / x) ~= tangent phi &&
  ((x**2) + (y**2)) ~= ((sine theta)**2) &&
  z ~= cosine theta &&
  (((x**2) + (y**2)) / (z**2)) ~= ((tangent theta)**2)

prop_from_spherical :: Double -> Double -> T.Property
prop_from_spherical theta phi = all validAngle [theta, phi] ==>
                                spherical_relations x y z x' y' theta' phi'
  where [theta', phi'] = map Degrees [theta, phi]
        Triple x  y  z = fromSphericalCoords theta' phi' :: Vec3
        Triple x' y' _ = normalize $ Triple x y 0

prop_to_spherical :: Vec3 -> T.Property
prop_to_spherical vec3 = norm2 vec3 /= 0 ==>
  spherical_relations x y z x' y' theta phi
  where Triple x  y  z = normalize vec3
        Triple x' y' _ = normalize $ Triple x y 0
        (theta, phi) = toSphericalCoords vec3

prop_spherical :: Double -> Double -> T.Property
prop_spherical phi theta = 0 <= phi && phi <= 90 && 0 < theta && theta < 90 ==>
  let (phi', theta') = toSphericalCoords $ fromSphericalCoords (Degrees phi) (Degrees theta) 
  in Degrees phi ~= phi' && Degrees theta ~= theta'

prop_reflect seed vector@(Triple x y z) z' =
  z < 0 && z' > 0 ==> tAnd $ liftA2 (~=) (reflect seed object vector) (normalize $ Triple x y (-z))
  where object = Object { _color = black
                        , _name = "test object"
                        , _light = False
                        , _reflective = True
                        , _form = InfinitePlane { _normal = Triple 0 0 z', _point = pure 0 }
                        }
  

return []
runTests = $quickCheckAll

main = do runTests
