{-# LANGUAGE TemplateHaskell #-}

import Data.Angle
import Debug.Trace
import Lib
import Object
       (Point(..), Vector(..), Ray(..), march, distanceFrom', Form(..))
import qualified Params
import qualified System.Random as Random
import Test.QuickCheck (quickCheck, quickCheckAll, (==>))
import qualified Test.QuickCheck as T
import Triple
import Util

tolerance :: Double
tolerance = 1e-8

(~=) :: Double -> Double -> Bool
(~=) = aeq tolerance

(^~=) :: Vec3 -> Vec3 -> Bool
(^~=) = vecAeq tolerance

propCross1 :: Vec3 -> Vec3 -> T.Property
propCross1 v1 v2 =
  not (any ((0 ~=) . norm2) [v1, v2]) ==> ((v1 `dot` crossProduct) ~= 0) &&
  ((v2 `dot` crossProduct) ~= 0)
  where
    crossProduct = v1 `cross` v2

propCross2 :: Vec3 -> Bool
propCross2 v = 0 `cross` v ^~= pure 0 && v `cross` 0 ^~= pure 0

propNorm2 :: Bool
propNorm2 = norm2 (Triple 3 4 5) ~= 7.0710678118654755

propNormalize1 :: Vec3 -> T.Property
propNormalize1 vec =
  not (norm2 vec ~= 0) ==> norm2 (normalize vec) ~= 1 && x ~= y && y ~= z
  where
    Triple x y z = vec / (normalize vec)

propNormalize2 :: Bool
propNormalize2 = norm2 (normalize $ Triple 0 0 0) == 0

sphericalRelations
  :: Double
  -> Double
  -> Double
  -> Double
  -> Double
  -> Degrees Double
  -> Degrees Double
  -> Bool
sphericalRelations x y z x' y' theta phi =
  x' ~= cosine phi && y' ~= sine phi && (y / x) ~= tangent phi &&
  ((x ** 2) + (y ** 2)) ~=
  ((sine theta) ** 2) &&
  z ~=
  cosine theta &&
  (((x ** 2) + (y ** 2)) / (z ** 2)) ~=
  ((tangent theta) ** 2)

propToSpherical1 :: Vec3 -> T.Property
propToSpherical1 vec3 =
  not (norm2 vec3 ~= 0) ==> sphericalRelations x y z x' y' theta phi
  where
    Triple x y z = normalize vec3
    Triple x' y' _ = normalize $ Triple x y 0
    (theta, phi) = toSphericalCoords vec3

propFromSpherical1 :: Double -> Double -> T.Property
propFromSpherical1 theta phi =
  all ((0, False, 180, True) `contains`) [theta, phi] ==>
  sphericalRelations x y z x' y' theta' phi'
  where
    [theta', phi'] = map Degrees [theta, phi]
    Triple x y z = fromSphericalCoords theta' phi' :: Vec3
    Triple x' y' _ = normalize $ Triple x y 0

propFromSpherical2 :: Bool
propFromSpherical2 = fromSphericalCoords 0 0 ^~= Triple 0 0 1

propToSpherical2 :: Bool
propToSpherical2 = not $ any isNaN [phi, theta]
  where
    (Degrees phi, Degrees theta) = toSphericalCoords (pure 0)

propSpherical1 :: Double -> Double -> T.Property
propSpherical1 theta phi =
  (0, False, 180, True) `contains` theta && (0, True, 360, True) `contains` phi ==>
  Degrees theta ~=
  theta' &&
  Degrees phi ~=
  phi'
  where
    (~=) = aeq 1e-13
    (theta', phi') =
      toSphericalCoords $ fromSphericalCoords (Degrees theta) (Degrees phi)

propSpherical2 :: Vec3 -> T.Property
propSpherical2 vec = not (norm2 vec ~= 0) ==> (normalize vec) ^~= vec'
  where
    (theta, phi) = toSphericalCoords vec
    vec' = fromSphericalCoords theta phi

propRotateAbs1 :: Double -> Double -> T.Property
propRotateAbs1 theta phi =
  (0, True, 180, True) `contains` theta && (0, True, 360, True) `contains` phi ==>
  (rotateAbs (Triple 0 0 1) theta' phi') ^~=
  (fromSphericalCoords theta' phi')
  where
    [theta', phi'] = map Degrees [theta, phi]

propRotateAbs2 :: Double -> Double -> Vec3 -> T.Property
propRotateAbs2 theta phi vector =
  (0, True, 180, True) `contains` theta && (0, True, 360, True) `contains` phi ==>
  norm2 vector ~=
  norm2 vector'
  where
    [theta', phi'] = map Degrees [theta, phi]
    vector' = rotateAbs vector theta' phi'

propRotateAbs3 :: Double -> Double -> Vec3 -> Vec3 -> T.Property
propRotateAbs3 theta phi vector1 vector2 =
  (0, True, 180, True) `contains` theta && (0, True, 360, True) `contains` phi ==>
  (vector1 `dot` vector2) ~=
  (vector1' `dot` vector2')
  where
    [theta', phi'] = map Degrees [theta, phi]
    [vector1', vector2'] =
      map (\v -> rotateAbs v theta' phi') [vector1, vector2]

propRotateRel :: Double -> Double -> Vec3 -> T.Property
propRotateRel theta phi vector =
  (0, True, 180, True) `contains` theta && (0, True, 360, True) `contains` phi &&
  not (norm2 vector ~= 0) ==>
  cosine theta' ~=
  (normalize vector `dot` normalize vector')
  where
    [theta', phi'] = map Degrees [theta, phi]
    vector' = rotateRel theta' phi' vector

propSpecular :: Vec3 -> Vec3 -> T.Property
propSpecular vector normal =
  not (any ((0 ~=) . norm2) [vector, normal]) ==>
  -- angle of incidence equals angle of reflection
  (normalize (-vector) `dot` normalize normal) ~=
  (normalize vector' `dot` normalize normal) &&
  (projOntoSurface vector) ^~=
  (projOntoSurface vector')
  where
    (vector', _) = specular (Random.mkStdGen 0) 0 vector normal
    normal' = normalize normal
    projOntoSurface v = v - fmap (v `dot` normal' *) normal'

defaultRay =
  Ray
  { _origin = Point $ pure 0
  , _vector = Vector $ pure 0
  , _gen = Random.mkStdGen 0
  , _lastStruck = Nothing
  }

propDistanceFrom' :: Vec3 -> Vec3 -> Vec3 -> Vec3 -> T.Property
propDistanceFrom' origin vector normal point =
  not (any ((0 ~=) . norm2) [origin, vector, normal, point]) ==>
  (intersection - point) `dot`
  normal ~=
  0
  where
    intersection = march ray distance
    ray = defaultRay {_origin = Point origin, _vector = Vector vector}
    Just distance =
      distanceFrom' ray $ InfinitePlane (Point point) (Vector normal)

propRandomRangeList :: Float -> Float -> Float -> Float -> Int -> T.Property
propRandomRangeList l1 h1 l2 h2 seed =
  l1 > h1 && l2 > h2 ==> fst (randomRangeList gen [(l1, h1), (l2, h2)]) ==
  [o1, o2]
  where
    gen = Random.mkStdGen seed
    (o1, gen') = Random.randomR (l1, h1) gen :: (Float, Random.StdGen)
    (o2, gen'') = Random.randomR (l2, h2) gen' :: (Float, Random.StdGen)

main = do
  putStrLn "propCross1"
  quickCheck propCross1
  putStrLn "propCross2"
  quickCheck propCross2
  putStrLn "propRandomRangeList"
  quickCheck propRandomRangeList
  putStrLn "propDistanceFrom'"
  quickCheck propDistanceFrom'
  putStrLn "propNormalize1"
  quickCheck propNormalize1
  putStrLn "propRotateAbs1"
  quickCheck propRotateAbs1
  putStrLn "propRotateAbs2"
  quickCheck propRotateAbs2
  putStrLn "propRotateAbs3"
  quickCheck propRotateAbs3
  putStrLn "propRotateRel"
  quickCheck propRotateRel
  putStrLn "propSpherical1"
  quickCheck propSpherical1
  putStrLn "propSpherical2"
  quickCheck propSpherical2
  putStrLn "propToSpherical1"
  quickCheck propToSpherical1
  putStrLn "propToSpherical2"
  quickCheck propToSpherical2
  putStrLn "propFromSpherical1"
  quickCheck propFromSpherical1
  putStrLn "propFromSpherical2"
  quickCheck propFromSpherical2
  putStrLn "propSpecular"
  quickCheck propSpecular
  -- one offs
  putStrLn "propNorm2"
  quickCheck propNorm2
  putStrLn "propNormalize2"
  quickCheck propNormalize2
