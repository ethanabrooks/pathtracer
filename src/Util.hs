{-# LANGUAGE MultiParamTypeClasses #-}

module Util
  ( black
  , white
  , flatten
  , mapIndex
  , toSphericalCoords
  , fromSphericalCoords
  , reshape
  , expandDim
  , aeq
  , vecAeq
  , rotateAbs
  , rotateRel
  , contains
  , randomAngle
  , randomRangeList
  , fromTripleArray
  , toTripleArray
  , startingCanvasM
  ) where

import Control.Applicative
import Data.Angle
       (Degrees(..), arctangent, arccosine, cosine, sine)
import Data.Array.Repa
       ((:.)(..), Array, D, DIM1, DIM2, DIM3, U, Z(..), (!))
import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.Shape as S
import Data.Fixed (mod')
import qualified Params
import qualified System.Random as Random
import Triple
       (Vec3, Triple(..), tripleToList, listToTriple, normalize, norm2,
        tAnd)

instance Functor Degrees where
  fmap f (Degrees x) = Degrees (f x)

black = pure 0 :: Vec3

white = pure 1 :: Vec3

inferMissing
  :: (Show a, Integral a)
  => [a] -> [a] -> [a]
inferMissing list listWithNeg
  | not valid =
    error
      ((show list) ++ " and " ++ (show listWithNeg) ++ " are not valid inputs.")
  | valid = result
  where
    valid = (product result == product list) && (all (> 0) result)
    missingVal = product list `quot` product (filter (>= 0) listWithNeg)
    result =
      map
        (\x ->
           if x < 0
             then missingVal
             else x)
        listWithNeg

fromTripleArray
  :: R.Source r (Triple a)
  => Array r DIM2 (Triple a) -> Array D DIM3 a
fromTripleArray array =
  R.fromFunction
    (Z :. rows :. cols :. 3)
    (\(Z :. i :. j :. k) -> tripleToList (array ! (Z :. i :. j)) !! k)
  where
    (Z :. rows :. cols) = R.extent array

toTripleArray
  :: R.Source r a
  => Array r DIM3 a -> Array D DIM2 (Triple a)
toTripleArray array =
  R.fromFunction
    (Z :. rows :. cols)
    (\(Z :. i :. j) -> listToTriple [array ! (Z :. i :. j :. k) | k <- [0 .. 2]])
  where
    (Z :. rows :. cols :. _) = R.extent array

mapIndex
  :: (S.Shape sh', R.Source r a)
  => (sh' -> b) -> Array r sh' a -> Array D sh' b
mapIndex f array = R.traverse array id $ const f

reshape
  :: (R.Source r1 e, S.Shape sh1, S.Shape sh2)
  => [Int] -> Array r1 sh1 e -> Array D sh2 e
reshape shape array = R.reshape (S.shapeOfList shape') array
  where
    shape' = inferMissing (S.listOfShape (R.extent array)) shape

flatten
  :: (R.Source r1 e, S.Shape sh1, S.Shape sh2)
  => Array r1 sh1 e -> Array D sh2 e
flatten array = reshape [-1] array

expandDim
  :: (R.Source r1 e, S.Shape sh1, S.Shape sh2)
  => Int -> Array r1 sh1 e -> Array D sh2 e
expandDim dim array = R.reshape shape array
  where
    shape = S.shapeOfList . (insertAt dim 1) . S.listOfShape $ R.extent array

insertAt :: Int -> a -> [a] -> [a]
insertAt n x list = (take n list) ++ [x] ++ (drop n list)

arctan2 :: Double -> Double -> Degrees Double
arctan2 x y = (`mod'` 360) <$> arctan'
  where
    arctan = arctangent $ y / x
    arctan'
      | x < 0 = arctan + Degrees 180
      | x >= 0 = arctan

toSphericalCoords :: Vec3 -> (Degrees Double, Degrees Double)
toSphericalCoords vec
  | all (0 ==) [x, y] && z >= 0 = (0, 0)
  | all (0 ==) [x, y] && z < 0 = (Degrees 180, 0)
  | otherwise = (theta, phi)
  where
    Triple x y z = normalize vec
    theta = arccosine z
    phi = arctan2 x y

fromSphericalCoords :: Degrees Double -> Degrees Double -> Vec3
fromSphericalCoords theta phi = Triple x y z
  where
    x = cosine phi * sine theta
    y = sine phi * sine theta
    z = cosine theta

rotateAbs :: Vec3 -> Degrees Double -> Degrees Double -> Vec3
rotateAbs (Triple x y z) theta phi = Triple x' y' z'
  where
    x' =
      cosine theta * cosine phi * x - sine phi * y + sine theta * cosine phi * z
    y' =
      sine phi * cosine theta * x + cosine phi * y + sine phi * sine theta * z
    z' = -sine theta * x + cosine theta * z

rotateRel :: Degrees Double -> Degrees Double -> Vec3 -> Vec3
rotateRel theta phi vector = (* length) <$> rotateAbs vector' theta' phi'
  where
    vector' = fromSphericalCoords theta phi
    (theta', phi') = toSphericalCoords vector
    length = norm2 vector

aeq
  :: (Num a, Ord a)
  => a -> a -> a -> Bool
aeq tolerance a b = (a - b) * (a - b) < tolerance

vecAeq
  :: (Ord a, Num a)
  => a -> Triple a -> Triple a -> Bool
vecAeq tolerance a b = tAnd $ liftA2 (aeq tolerance) a b

contains
  :: Ord t
  => (t, Bool, t, Bool) -> t -> Bool
contains (low, lowInclusive, high, highInclusive) x = aboveLow && belowHigh
  where
    aboveLow
      | lowInclusive = low <= x
      | otherwise = low < x
    belowHigh
      | highInclusive = x <= high
      | otherwise = x < high

randomAngle
  :: (Random.RandomGen b, Random.Random x, Show x)
  => b -> (x, x) -> (Degrees x, b)
randomAngle gen range = (Degrees angle, gen')
  where
    (angle, gen') = Random.randomR range gen

randomRangeList
  :: (Random.RandomGen b, Random.Random a, Show a)
  => b -> [(a, a)] -> ([a], b)
randomRangeList firstGen ranges = (reverse randoms, lastGen)
  where
    (randoms, lastGen) =
      foldl
        (\(xs, gen) range ->
           let (x, gen') = Random.randomR range gen
           in (x : xs, gen'))
        ([], firstGen)
        ranges
