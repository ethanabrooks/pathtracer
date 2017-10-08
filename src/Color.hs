{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Color
  ( Color
  , newColor
  , colorToTriple
  , colorToWord32
  , tripleToColor
  , white
  , black
  ) where

import Data.Array.Accelerate (Lift(..), Unlift(..), Plain)
import Data.Array.Accelerate.Array.Sugar
       (Elt(..), EltRepr, Tuple(..))
import Data.Array.Accelerate.Product (IsProduct(..))
import Data.Array.Accelerate.Smart
import Data.Bits
import Data.Range.Range (Range(..))
import Data.Word (Word8, Word32)
import Triple (Vec3, Triple(..))
import Util (clamp)

newtype Color a =
  Color (Triple a)
  deriving (Eq, Show, Num, Functor, Applicative)

type instance EltRepr (Color a) = EltRepr (Triple a)

instance Elt a =>
         Elt (Color a) where
  eltType _ = eltType (undefined :: Triple a)
  toElt p = Color $ toElt p
  fromElt (Color triple) = fromElt triple

instance (Elt a, Lift Exp a, Elt (Plain a)) =>
         Lift Exp (Color a) where
  type Plain (Color a) = Plain (Triple a)
  lift (Color triple) = lift triple

newColor a b c = Color $ Triple a b c

black = Color $ pure 0 :: Color Double

white = Color $ pure 1 :: Color Double

colorClamp
  :: (Functor f, Ord a, Num a)
  => f a -> f a
colorClamp = fmap (clamp 0 1)

tripleToColor
  :: RealFrac a
  => Triple a -> Color a
tripleToColor = Color . colorClamp

colorToTriple
  :: RealFrac a
  => Color a -> Triple a
colorToTriple (Color triple) = colorClamp triple

colorToWord32
  :: RealFrac a
  => Color a -> Word32
colorToWord32 = foldr concat 0 . colorToTriple
  where
    concat colorValue word =
      shift word bitsPerByte .|. round (colorValue * 0xFF)
    bitsPerByte = 8
