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

import Data.Array.Repa
       ((:.)(..), Array, D, DIM1, DIM2, U, Z(..), (!))
import Lib (traceCanvas)
import qualified Params
import Triple (Vec3, Triple(..))
import Util (black, flatten, reshape)

toImage
  :: (R.Source r Vec3)
  => Array r DIM2 Vec3 -> P.DynamicImage
toImage canvas =
  P.ImageRGB8 $ P.generateImage fromCoords Params.imgHeight Params.imgWidth
  where
    fromCoords i j =
      convertToPixel . (fmap round) . ((255 *) <$>) $ getColor i j
      where
        getColor i j = canvas ! (Z :. i :. j)
        convertToPixel (Triple r g b) = P.PixelRGB8 r g b

blackCanvas :: Array D DIM2 Vec3
blackCanvas =
  R.fromFunction (Z :. Params.imgHeight :. Params.imgWidth) $ const black

main :: IO ()
main = (P.savePngImage "image.png" . toImage) canvas'
  where
    (_, flatCanvas) =
      iterate
        (\(iteration, canvas) -> (iteration + 1, traceCanvas iteration canvas))
        (0, flatten blackCanvas) !!
      Params.numIters
    canvas' = reshape [Params.imgHeight, Params.imgWidth] flatCanvas
