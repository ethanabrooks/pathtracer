{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Main
  (
  ) where

import Data.Array.Accelerate
       (Acc, Z(..), (:.)(..), Elt(..), Lift(..), Unlift(..), Plain)
import Data.Array.Accelerate.Array.Sugar
       (Elt(..), EltRepr, Tuple(..), TupleRepr)
import Data.Array.Accelerate.Product (TupleIdx(..), IsProduct(..))
import Data.Array.Accelerate.Smart
import Data.Typeable (Typeable)
import Prelude as P

{-dotp :: Acc (A.Vector Float) -> Acc (A.Vector Float) -> Acc (A.Scalar Float)-}
{-dotp xs ys = A.fold (+) 0 (A.zipWith (*) xs ys)-}
data Triple a =
  Triple a
         a
         a
  deriving (Show, P.Eq, Typeable)

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
  type ProdRepr (Triple a) = ProdRepr (a, a, a)
  fromProd cst (Triple x y z) = fromProd cst (x, y, z)
  toProd cst p =
    let (x, y, z) = toProd cst p
    in Triple x y z
  prod cst _ = prod cst (undefined :: (Triple a))

instance (Lift Exp a, Elt (Plain a)) =>
         Lift Exp (Triple a) where
  type Plain (Triple a) = Triple (Plain a)
  lift (Triple x y z) =
    Exp $ Tuple (NilTup `SnocTup` lift x `SnocTup` lift y `SnocTup` lift z)

instance Elt a =>
         Unlift Exp (Triple (Exp a)) where
  unlift p =
    let x = Exp $ SuccTupIdx ZeroTupIdx `Prj` p
        y = Exp $ ZeroTupIdx `Prj` p
        z = Exp $ ZeroTupIdx `Prj` p
    in Triple x y z
{-
main :: P.IO ()
main = do
  let v1 = A.use (A.fromList (Z :. 3) [0 ..] :: A.Vector Float)
  let v2 = A.map (+ 1) v1
  P.print . run $ dotp v1 v2
  print $ A.toList v2-}
