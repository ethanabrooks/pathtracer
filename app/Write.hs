{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Main
  ( main
  , Vec3
  ) where

import qualified Codec.Picture as P
import qualified Data.Array.Repa as R

import Conversion (repaToImage, toImage)
import Data.Array.Repa
       ((:.)(..), Array, D, DIM1, DIM2, U, Z(..), (!))
import Lib (traceCanvas)
import qualified Params
import Triple (Vec3, Triple(..))
import Util (black, flatten, reshape, fromTripleArray)

blackCanvas :: Array D DIM2 Vec3
blackCanvas =
  R.fromFunction (Z :. Params.imgHeight :. Params.imgWidth) $ const black

main :: IO ()
main = (P.savePngImage "image.png" . P.ImageRGB8 . toImage) canvas'
  where
    (_, flatCanvas) =
      iterate
        (\(iteration, canvas) -> (iteration + 1, traceCanvas iteration canvas))
        (0, flatten blackCanvas) !!
      Params.numIters
    canvas' = reshape [Params.imgHeight, Params.imgWidth] flatCanvas
