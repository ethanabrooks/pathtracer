{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Main
  ( main
  ) where

import Data.Array.Accelerate as A
import Data.Array.Accelerate.Array.Sugar
       (Elt(..), EltRepr, Tuple(..))
import Data.Array.Accelerate.Product (TupleIdx(..), IsProduct(..))
import Data.Array.Accelerate.Smart

import Data.Array.Accelerate (Acc, Z(..), (:.)(..), Elt(..))
import qualified Data.Array.Accelerate.Data.Monoid as AM
import Data.Array.Accelerate.Interpreter (run)
import Data.Typeable
import Prelude (fromInteger) -- ghc < 8 bug

import qualified Prelude as P

dotp :: Acc (A.Vector Float) -> Acc (A.Vector Float) -> Acc (A.Scalar Float)
dotp xs ys = A.fold (+) 0 (A.zipWith (*) xs ys)

data Triple a =
  Triple a
         a
         a
  deriving (P.Show, P.Eq, Typeable)

{-
type instance EltRepr (Triple a) = EltRepr (a, a, a)

instance Elt a =>
         Elt (Triple a) where
  eltType (_ :: Triple a) = eltType (undefined :: (a, a, a))
  toElt p =
    let (x, y, z) = toElt p
    in Triple x y z
  fromElt (Triple x y z) = fromElt (x, y, z)

instance Elt a =>
         IsProduct Elt (Triple a) where
  type ProdRepr (Triple a) = (((), a), a)
  fromProd _ (Triple x y z) = ((((), x), y), z)
  toProd _ ((((), x), y), z) = Triple x y z
  prod cst _ = prod cst (undefined :: (a, a, a))

instance (Lift Exp a, Elt (Plain a)) =>
         Lift Exp (Triple a) where
  type Plain (Triple a) = Triple (Plain a)
  lift (Triple x y z) =
    Exp . Tuple $ NilTup `SnocTup` lift x `SnocTup` lift y `SnocTup` lift z

instance Elt a =>
         Unlift Exp (Triple (Exp a)) where
  unlift p =
    let x = Exp $ SuccTupIdx ZeroTupIdx `Prj` p
        y = Exp $ ZeroTupIdx `Prj` p
        z = Exp $ ZeroTupIdx `Prj` p
    in Triple x y z
-}
main :: P.IO ()
main = do
  let v1 = A.use (A.fromList (Z :. 3) [0 ..] :: A.Vector Float)
  let v2 = A.map (+ 1) v1
  P.print . run $ dotp v1 v2
  {-print $ A.toList v2-}
