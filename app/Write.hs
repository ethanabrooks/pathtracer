module Main
  ( main
  ) where

import qualified Codec.Picture as P
import Control.Arrow
import Conversion (repa3ToImage)
import Lib (tracedCanvas)
import qualified Params

main :: IO ()
main =
  tracedCanvas >>= (repa3ToImage >>> P.ImageRGB8 >>> P.savePngImage "image.png")
