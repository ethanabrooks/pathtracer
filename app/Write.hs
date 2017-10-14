module Main
  ( main
  ) where

import qualified Codec.Picture as P

import Lib (traces)
import qualified Params
import Util (repa3ToImage)

main :: IO ()
main = do
  canvasDim3 <- traces !! Params.numIters
  (P.savePngImage "image.png" . P.ImageRGB8 . repa3ToImage) canvasDim3
