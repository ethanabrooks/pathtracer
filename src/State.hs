{-# LANGUAGE Strict #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module State
  {-( Triple(..)-}
  {-, Vec3-}
  {-, dot-}
  {-, cross-}
  {-, norm2-}
  {-, normalize-}
  {-)-}
 where

import Control.Applicative
import Test.QuickCheck (Arbitrary, arbitrary)

import Data.Array.Accelerate
       (Acc, Z(..), (:.)(..), Elt(..), Lift(..), Unlift(..), Plain)
import Data.Array.Accelerate.Array.Sugar
       (Elt(..), EltRepr, Tuple(..), TupleRepr)
import Data.Array.Accelerate.Product (TupleIdx(..), IsProduct(..))
import Data.Array.Accelerate.Smart
import Data.Hashable
import Data.Typeable (Typeable)
import Object (Ray)
import Prelude as P
import qualified System.Random as Random
import Text.Read (read)

data State =
  State Ray
        Random.StdGen
  deriving (Show)
{-
type instance EltRepr State = EltRepr (Ray, (Int, Int))

instance Elt State where
  eltType _ = eltType (undefined :: (Ray, (Int, Int)))
  toElt p =
    let (ray, string) = toElt p
    in State ray (read string)
  fromElt (State ray gen) = fromElt (ray, show gen)
  -}
