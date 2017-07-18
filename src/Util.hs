module Util ( black
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
            ) where

import Triple
import Data.Array.Repa (Array, DIM1, DIM2, U, D, Z (..), (:.)(..), (!))
import qualified Data.Array.Repa     as R -- for Repa
import qualified Data.Array.Repa.Shape as S
import Data.Angle
import Data.Fixed
import Debug.Trace
import System.Random
import Control.Applicative
import System.IO.Unsafe



instance Functor Degrees where
  fmap f (Degrees x) = Degrees (f x)


black = pure 0 :: Vec3
white = pure 1 :: Vec3


inferMissing :: (Show a, Integral a) => [a] -> [a] -> [a]
{-# INLINE inferMissing #-}
inferMissing list listWithNeg
  | not valid = error ((show list) ++ " and " ++ (show listWithNeg) ++ " are not valid inputs.")
  | valid     = result
    where valid      = (product result == product list) && (all (> 0) result)
          missingVal = product list `quot` product (filter (>= 0) listWithNeg)
          result     = map (\x -> if x < 0 then missingVal else x) listWithNeg


mapIndex ::
  (S.Shape sh', R.Source r a) =>
  (sh' -> b) -> Array r sh' a -> Array D sh' b
{-# INLINE mapIndex #-}
mapIndex f array = R.traverse array id $ const f


reshape :: (R.Source r1 e, S.Shape sh1, S.Shape sh2) => [Int] -> Array r1 sh1 e -> Array D sh2 e
{-# INLINE reshape #-}
reshape shape array = R.reshape (S.shapeOfList shape') array
  where shape' = inferMissing (S.listOfShape (R.extent array)) shape


flatten :: (R.Source r1 e, S.Shape sh1, S.Shape sh2) => Array r1 sh1 e -> Array D sh2 e
{-# INLINE flatten #-}
flatten array = reshape [-1] array


expandDim :: (R.Source r1 e, S.Shape sh1, S.Shape sh2) => Int -> Array r1 sh1 e -> Array D sh2 e
{-# INLINE expandDim #-}
expandDim dim array = R.reshape shape array
  where shape            = S.shapeOfList . (insertAt dim 1) . S.listOfShape $ R.extent array


insertAt :: Int -> a -> [a] -> [a]
{-# INLINE insertAt #-}
insertAt n x list = (take n list) ++ [x] ++ (drop n list)


arctan2 :: Double -> Double -> Degrees Double
{-# INLINE arctan2 #-}
arctan2 x y = fmap (`mod'` 360) arctan'
  where arctan = arctangent $ y / x
        arctan' | x <  0 = arctan + Degrees 180 
                | x >= 0 = arctan


toSphericalCoords :: Vec3 -> (Degrees Double, Degrees Double)
{-# INLINE toSphericalCoords #-}
toSphericalCoords vec | all (0 ==) [x, y] && z >= 0 = (0, 0)
                      | all (0 ==) [x, y] && z <  0 = (Degrees 180, 0)
                      | otherwise         = (theta, phi)
  where Triple x y z  = normalize vec
        theta         = arccosine z
        phi           = arctan2 x y 
  

fromSphericalCoords :: Degrees Double -> Degrees Double -> Vec3
{-# INLINE fromSphericalCoords #-}
fromSphericalCoords theta phi = Triple x y z
  where x = cosine phi * sine theta
        y = sine phi * sine theta
        z = cosine theta


rotateAbs :: Vec3 -> Degrees Double -> Degrees Double -> Vec3
{-# INLINE rotateAbs #-}
rotateAbs (Triple x y z) theta phi = Triple x' y' z'
  where x' = cosine theta * cosine phi * x - sine phi * y + sine theta * cosine phi * z
        y' = sine phi * cosine theta * x + cosine phi * y + sine phi * sine theta * z
        z' = -sine theta * x + cosine theta * z


rotateRel :: Degrees Double -> Degrees Double -> Vec3 -> Vec3
{-# INLINE rotateRel #-}
rotateRel theta phi vector = fmap (* length) $ rotateAbs vector' theta' phi'
  where vector' = fromSphericalCoords theta phi
        (theta', phi') = toSphericalCoords vector
        length  = norm2 vector


aeq :: (Num a, Ord a) => a -> a -> a -> Bool
{-# INLINE aeq #-}
aeq tolerance a b = (a - b) * (a - b) < tolerance


vecAeq :: (Ord a, Num a) => a -> Triple a -> Triple a -> Bool
{-# INLINE vecAeq #-}
vecAeq tolerance a b = tAnd $ liftA2 (aeq tolerance) a b


contains :: Ord t => (t, Bool, t, Bool) -> t -> Bool
{-# INLINE contains #-}
contains (low, lowInclusive, high, highInclusive) x = aboveLow && belowHigh
  where aboveLow  | lowInclusive  = low <= x
                  | otherwise     = low < x
        belowHigh | highInclusive = x <= high
                  | otherwise     = x < high
  

randomAngle :: (RandomGen b, Random x, Show x) => b -> (x, x) -> (Degrees x, b)
{-# INLINE randomAngle #-}
randomAngle gen range = (Degrees angle, gen')
  where (angle, gen') = randomR range gen

randomRangeList :: (RandomGen b, Random a, Show a) => b -> [(a, a)] -> ([a], b)
{-# INLINE randomRangeList #-}
randomRangeList firstGen ranges = (reverse randoms, lastGen)
  where (randoms, lastGen) =  foldl (\(xs, gen) range -> let (x, gen') = randomR range gen
                                                       in (x:xs, gen'))
                            ([], firstGen) ranges

