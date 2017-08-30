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

import Conversion (repa2ToImage, repa1ToText)
import Data.Array.Repa
       ((:.)(..), Array, D, DIM1, DIM2, U, Z(..), (!))
import Lib (traceCanvas)
import qualified Params
import Triple (Vec3, Triple(..))
import Util (black, flatten, reshape, fromTripleArray, blackCanvas)

main :: IO ()
main = do
  (P.savePngImage "image.png" . P.ImageRGB8 . repa2ToImage) canvas'
  {-putStrLn . show $ repa1ToText flatCanvas-}
  where
    (_, flatCanvas) =
      iterate traceCanvas (0, flatten blackCanvas) !! Params.numIters
    canvas' = reshape [Params.height, Params.width] flatCanvas
