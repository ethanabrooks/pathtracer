{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TupleSections #-}

module Main
  ( main
  , Vec3
  ) where

import qualified Codec.Picture as P
import qualified Data.Array.Repa as R

import Conversion (repa3ToImage, repa1ToText)
import Data.Array.Repa
       ((:.)(..), Array, D, DIM1, DIM2, U, Z(..), (!))
import Lib (traceCanvas)
import qualified Params
import Triple (Vec3, Triple(..))
import Util
       (black, flatten, reshape, fromTripleArray, startingCanvasM)

main :: IO ()
main = do
  let (_, canvasDim3M) =
        iterate traceCanvas (0, startingCanvasM) !! Params.numIters
  canvasDim3 <- canvasDim3M
  (P.savePngImage "image.png" . P.ImageRGB8 . repa3ToImage) canvasDim3
