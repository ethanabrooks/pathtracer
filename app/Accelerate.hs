{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ExistentialQuantification #-}

module Main where

import Color (Color, black, tripleToColor)
import Data.Array.Accelerate
       (Acc, Z(..), (:.)(..), Elt(..), Lift(..), Unlift(..), Plain, DIM1,
        DIM2, Array)

import qualified Data.Array.Accelerate as A
import Data.Array.Accelerate.Array.Sugar
       (Elt(..), EltRepr(..), Tuple(..), TupleRepr)
import Data.Array.Accelerate.Interpreter (run)
import Data.Array.Accelerate.Product (TupleIdx(..), IsProduct(..))
import Data.Array.Accelerate.Smart (constant, Exp)
import Data.Typeable (Typeable)
import Data.Word
import Graphics.Gloss.Accelerate.Data.Picture (bitmapOfArray)
import Graphics.Gloss.Data.Color (white)
import Graphics.Gloss.Data.Display (Display(..))
import Graphics.Gloss.Data.Picture (Picture)
import Graphics.Gloss.Interface.Pure.Display (display)
import qualified Params
import Prelude as P
import Ray (Ray(..))
import State (State(..))
import qualified System.Random as Random
import Triple (normalize, Triple(..))
import Util (Point(..), Vector(..))

black' :: Acc (Array DIM2 Word32)
black' = A.fill (constant (Z :. 1000 :. 1000)) 0xFF0000FF

type Color' = Color Double

blackCanvas :: Acc (Array DIM2 (Color Double))
blackCanvas =
  A.fill (constant (Z :. Params.height :. Params.width)) $ constant black

rayFromCamToPixel :: Random.StdGen -> DIM2 -> Ray
rayFromCamToPixel gen (Z :. i :. j) =
  Ray
  { _origin = Point $ pure 0
  , _vector = Vector $ normalize $ Triple i' j' Params.cameraDepth
  , _gen = gen
  , _lastStruck = Nothing
  }
  where
    i' = fromIntegral Params.height / 2 - fromIntegral i
    j' = fromIntegral j - fromIntegral Params.width / 2

f
  :: Elt a
  => Exp a -> Exp a -> Exp a -> Exp (Triple a)
f a b c = unlift $ A.lift3 triple a b c
  where
    triple :: Exp a -> Exp a -> Exp a -> Triple (Exp a)
    triple = Triple

f1
  :: (Elt a, Elt b)
  => (Exp a -> Exp b -> (Exp a, Exp b)) -> Exp a -> Exp b -> Exp (a, b)
f1 f a b = unlift $ A.lift2 f a b

f2 :: Exp (Triple Double)
   -> Exp Random.StdGen
   -> Exp (Triple Double, (Int, Int))
f2 = A.lift2 tr

f0 :: Int -> Int -> Int
f0 = (+)

tr :: Triple (Exp Double) -> Exp Random.StdGen -> (Color Double, Random.StdGen)
tr = error ""

traceCanvas array = A.imap f blackCanvas
  where
    f :: Exp sh -> Exp (Color Double) -> Exp (Plain State)
    f sh color = error "" -- A.lift2 g (error "") (error "")
      where
        g :: Color Double -> Random.StdGen -> Exp (Plain State)
        g a b = A.lift2 f' a' b'
          where
            f'
              :: Exp (Plain (Color Double))
              -> Exp (Plain Random.StdGen)
              -> Plain State
            f' = error ""
            a' :: Exp (Plain (Color Double))
            a' = error ""
            b' :: Exp (Plain Random.StdGen)
            b' = error "" {-constant (State (color + A.constant color') gen')-}
        --color'' = A.lift1 (toProd 0) $ fromElt (color :: Color Double)
      {-where
        (color', gen') =
          traceRay Params.maxBounces white (rayFromCamToPixel gen sh) :: (Color Double, Random.StdGen)-}

main :: P.IO ()
main = print ""
  {-
  display window white pic
  where
    black' = run (black :: Acc (Array DIM2 Word32))
    window = InWindow "wind" (100, 100) (100, 100)
    pic = bitmapOfArray (black' :: Array DIM2 Word32) True :: Picture
  let v1 = A.use (A.fromList (Z :. 3) [0 ..] :: A.Vector Float)
  let v2 = A.map (+ 1) v1
  P.print . run $ dotp v1 v2
  print $ A.toList v2
  -}
