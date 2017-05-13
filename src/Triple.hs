module Triple ( Triple (..)
              , Vec3
              , RGB8
              , dot
              , norm2
              , normalize
              ) where

import qualified Codec.Picture as P
import qualified Data.Array.Repa     as R -- for Repa
import qualified Data.Array.Repa.Shape as S
import Data.Array.Repa (Array, DIM1, DIM2, DIM3, U, D, Z (..), (:.)(..),
                        (!), (++), (*^), (+^), (-^), (/^))
import Control.Applicative

data Triple a = Triple a a a
type Vec3 = Triple Double
type RGB8 = Triple P.Pixel8

tripleToTuple :: Triple t -> (t, t, t)
tripleToTuple (Triple a1 a2 a3) = (a1, a2, a3)

tripleToList :: Triple t -> [t]
tripleToList (Triple a1 a2 a3) = [a1, a2, a3]

tsum :: Num a => Triple a -> a
tsum = sum . tripleToList

dot a b = tsum $ a * b 

norm2 :: Floating a => Triple a -> a
norm2 (Triple x y z) = sqrt $ x^2 + y^2 + z^2

normalize :: Vec3 -> Vec3
normalize vector = fmap (/ norm) vector
  where norm = max (10^(-6)) $ norm2 vector

instance Show a => Show (Triple a) where
  show = show . tripleToTuple
  
instance Functor (Triple) where
  fmap f (Triple a1 a2 a3) = Triple (f a1) (f a2) (f a3)

instance Applicative (Triple) where
  pure a = Triple a a a
  Triple f1 f2 f3 <*> Triple a1 a2 a3 = Triple (f1 a1) (f2 a2) (f3 a3)

instance Num a => Num (Triple a) where 
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  abs = fmap abs
  signum = fmap signum
  fromInteger = fmap fromInteger . pure
  negate = fmap negate
