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
  let (_, flatCanvasM) =
        iterate traceCanvas (0, startingCanvasM) !! Params.numIters
  flatCanvas <- flatCanvasM
  let canvas' = reshape [Params.imgHeight, Params.imgWidth] flatCanvas
  (P.savePngImage "image.png" . P.ImageRGB8 . repa3ToImage) canvas'
