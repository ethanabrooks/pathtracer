module Main where

import Data.Array.Accelerate
       (fill, Acc, Z(..), (:.)(..), Elt(..), Lift(..), Unlift(..), Plain,
        DIM1, DIM2, Array)
import Data.Array.Accelerate.Array.Sugar
       (Elt(..), EltRepr, Tuple(..), TupleRepr)
import Data.Array.Accelerate.Interpreter (run)
import Data.Array.Accelerate.Product (TupleIdx(..), IsProduct(..))
import Data.Array.Accelerate.Smart
import Data.Typeable (Typeable)
import Data.Word
import Graphics.Gloss.Accelerate.Data.Picture (bitmapOfArray)
import Graphics.Gloss.Data.Color (white)
import Graphics.Gloss.Data.Display (Display(..))
import Graphics.Gloss.Data.Picture (Picture)
import Graphics.Gloss.Interface.Pure.Display (display)
import Prelude as P

black :: Acc (Array DIM2 Word32)
black = fill (constant (Z :. 1000 :. 1000)) 0xFF0000FF

main :: P.IO ()
main = display window white pic
  where
    black' = run (black :: Acc (Array DIM2 Word32))
    window = InWindow "wind" (100, 100) (100, 100)
    pic = bitmapOfArray (black' :: Array DIM2 Word32) True :: Picture
  {-
  let v1 = A.use (A.fromList (Z :. 3) [0 ..] :: A.Vector Float)
  let v2 = A.map (+ 1) v1
  P.print . run $ dotp v1 v2
  print $ A.toList v2
  -}
