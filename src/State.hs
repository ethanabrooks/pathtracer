{-# LANGUAGE Strict #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module State where

import Data.Array.Accelerate (Lift(..), Unlift(..), Plain)
import Data.Array.Accelerate.Array.Sugar
       (Elt(..), EltRepr, Tuple(..))
import Data.Array.Accelerate.Product (IsProduct(..))
import Data.Array.Accelerate.Smart
import Ray (Ray)
import qualified System.Random as Random
import Triple (Vec3)
import Util (Color(..))

data State =
  State Color
        Random.StdGen
  deriving (Show)

type EltReprState = (Vec3, Random.StdGen)

type instance EltRepr State = EltRepr EltReprState

instance Elt State where
  eltType _ = eltType (undefined :: EltReprState)
  toElt p =
    let (color, gen) = toElt p
    in State (Color color) gen
  fromElt (State (Color color) gen) = fromElt (color, gen)

instance IsProduct Elt State where
  type ProdRepr State = ProdRepr EltReprState
  fromProd cst (State (Color color) gen) = fromProd cst (color, gen)
  toProd cst p =
    let (color, gen) = toProd cst p
    in State (Color color) gen
  prod cst _ = prod cst (undefined :: EltReprState)

instance Lift Exp State where
  type Plain State = (Plain Vec3, Plain Random.StdGen)
  lift (State (Color color) gen) = Exp $ Tuple tuple
    where
      tuple = NilTup `SnocTup` lift color `SnocTup` lift gen
