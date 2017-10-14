{-# LANGUAGE Strict #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}

module State where

import Color (Color, colorToTriple)
import Data.Array.Accelerate (Lift(..), Unlift(..), Plain)
import Data.Array.Accelerate.Array.Sugar
       (Elt(..), EltRepr, Tuple(..))
import Data.Array.Accelerate.Product (IsProduct(..))
import Data.Array.Accelerate.Smart
import Ray (Ray)
import qualified System.Random as Random
import Triple (Vec3)

data State =
  State (Color Double)
        Random.StdGen
  deriving (Show)

type EltReprState = (Color Double, Random.StdGen)

type instance EltRepr State = EltRepr EltReprState

instance Elt State where
  eltType _ = eltType (undefined :: EltReprState)
  toElt :: EltRepr EltReprState -> State
  toElt p =
    let (color, gen) = toElt p
    in State color gen
  fromElt :: State -> EltRepr EltReprState
  fromElt (State color gen) = fromElt (color, gen)

instance IsProduct Elt State where
  type ProdRepr State = ProdRepr EltReprState
  fromProd :: proxy Elt -> State -> ProdRepr State
  fromProd cst (State color gen) = fromProd cst (color, gen)
  toProd :: proxy Elt -> ProdRepr State -> State
  toProd cst p =
    let (color, gen) = toProd cst p
    in State color gen
  prod cst _ = prod cst (undefined :: EltReprState)

instance Lift Exp State where
  type Plain State = (Plain Vec3, Plain Random.StdGen)
  lift :: State -> Exp (Plain State)
  lift (State color gen) = Exp $ Tuple tuple
    where
      tuple = NilTup `SnocTup` lift color `SnocTup` lift gen

getPixel :: State -> Color Double
getPixel (State color _) = color
