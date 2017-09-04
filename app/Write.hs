module Main
  ( main
  ) where

import qualified Codec.Picture as P

import Conversion (repa3ToImage)
import Lib (traces)
import qualified Params

main :: IO ()
main = do
  canvasDim3 <- traces !! Params.numIters
  (P.savePngImage "image.png" . P.ImageRGB8 . repa3ToImage) canvasDim3
