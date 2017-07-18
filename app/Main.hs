{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE FlexibleContexts #-}

module Main ( main
            , Vec3
            ) where

import qualified Codec.Picture.Types          as M
import qualified Data.Array.Repa              as R 
import qualified Data.Array.Repa.Shape        as S
import qualified Data.Array.Repa.Repr.Unboxed as U
import qualified Codec.Picture                as P

import Data.Array.Repa (Array, DIM1, DIM2, U, D, Z (..), (:.)(..), (!))
import System.Environment (getArgs)
import System.FilePath (replaceExtension)
import Object
import Triple
import Lib 
import Util 
import Control.Monad
import Control.Monad.Loops
import Debug.Trace
import System.Random


toImage :: (R.Source r Vec3) => Array r DIM2 Vec3 -> P.DynamicImage
toImage canvas = P.ImageRGB8 $ P.generateImage fromCoords imgHeight imgWidth
  where fromCoords i j = convertToPixel . (fmap round) . rescale $ getColor i j 
            where getColor i j = canvas ! (Z :. i :. j)
                  rescale = fmap (*255)
                  convertToPixel (Triple r g b) = P.PixelRGB8 r g b

addFrame :: (R.Source r Vec3) => Array r DIM2 Vec3 -> Array D DIM2 Vec3
addFrame canvas = R.zipWith color coords canvas 
  where color (Triple x y z) pixel = if x `elem` [0, imgHeight - 1] || y `elem` [0, imgWidth - 1]
                                     then Triple 255 255 255
                                     else canvas ! (Z :. x :. y)
        coords = R.fromFunction (Z :. imgHeight :. imgWidth) (\(Z :. i :. j) -> Triple i j 0)

main :: IO ()
main = (P.savePngImage "image.png" . toImage) $ reshape [imgHeight, imgWidth] canvas
    where (_, canvas) = iterate (uncurry mainLoop) (0, flatten blackCanvas) !! numIters
