{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Data.Array.Accelerate
       (Acc, Z(..), (:.)(..), Elt(..), Lift(..), Unlift(..), Plain)
import Data.Array.Accelerate.Array.Sugar
       (Elt(..), EltRepr, Tuple(..), TupleRepr)
import Data.Array.Accelerate.Product (TupleIdx(..), IsProduct(..))
import Data.Array.Accelerate.Smart
import Data.Typeable (Typeable)
import Prelude as P

main :: P.IO ()
main = print "hello"
  {-
  let v1 = A.use (A.fromList (Z :. 3) [0 ..] :: A.Vector Float)
  let v2 = A.map (+ 1) v1
  P.print . run $ dotp v1 v2
  print $ A.toList v2
  -}
