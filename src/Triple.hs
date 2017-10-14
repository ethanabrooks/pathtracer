{-# LANGUAGE Strict #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Triple
  ( Triple(..)
  , Vec3
  , dot
  , cross
  , norm2
  , normalize
  ) where

import Control.Applicative
import Test.QuickCheck (Arbitrary, arbitrary)

import Data.Typeable (Typeable)
import Prelude as P
import qualified System.Random as Random

data Triple a =
  Triple a
         a
         a
  deriving (Show, P.Eq, Typeable, Functor, Foldable, Traversable)

-- |Accelerate
instance Applicative Triple where
  pure a = Triple a a a
  Triple f1 f2 f3 <*> Triple a1 a2 a3 = Triple (f1 a1) (f2 a2) (f3 a3)

instance Num a =>
         Num (Triple a) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  abs = fmap abs
  signum = fmap signum
  fromInteger = fmap fromInteger . pure
  negate = fmap negate

instance Fractional a =>
         Fractional (Triple a) where
  fromRational = fmap fromRational . pure
  (/) = liftA2 (/)

instance Arbitrary a =>
         Arbitrary (Triple a) where
  arbitrary = do
    a1 <- arbitrary
    a2 <- arbitrary
    a3 <- arbitrary
    return $ Triple a1 a2 a3

type Vec3 = Triple Double

dot
  :: Num a
  => Triple a -> Triple a -> a
dot a b = sum $ a * b

cross
  :: Num a
  => Triple a -> Triple a -> Triple a
cross (Triple x1 y1 z1) (Triple x2 y2 z2) = Triple x y z
  where
    x = y1 * z2 - z1 * y2
    y = z1 * x2 - x1 * z2
    z = x1 * y2 - y1 * x2

norm2
  :: Floating a
  => Triple a -> a
norm2 = sqrt . sum . fmap (^ 2)

normalize :: Vec3 -> Vec3
normalize vector = (/ norm) <$> vector
  where
    norm = max (10 ** (-6)) $ norm2 vector
