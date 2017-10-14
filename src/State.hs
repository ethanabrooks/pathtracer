{-# LANGUAGE Strict #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}

module State where

import Color (Color, colorToTriple)
import Ray (Ray)
import qualified System.Random as Random
import Triple (Vec3)

data State =
  State (Color Double)
        Random.StdGen
  deriving (Show)

getPixel :: State -> Color Double
getPixel (State color _) = color
