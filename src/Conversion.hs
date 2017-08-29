{-# LANGUAGE FlexibleContexts #-}

module Conversion where

import qualified Codec.Picture as P
import Data.Array.Repa
       ((:.)(..), Array, D, DIM3, DIM1, DIM2, U, Z(..), (!), (+^))
import qualified Data.Array.Repa as R
import qualified Data.ByteString.Base64
import qualified Data.ByteString.Lazy.Char8
import qualified Data.Text.Encoding
import qualified Data.Text.Lazy as TL
import qualified Params
import Triple (Triple(..), Vec3, normalize, dot)

repaToImage
  :: (R.Source r Double)
  => Array r DIM3 Double -> P.Image P.PixelRGB8
repaToImage canvas = P.generateImage fromCoords Params.imgHeight Params.imgWidth
  where
    fromCoords i j =
      let [r, g, b] = [round $ canvas ! (Z :. i :. j :. k) | k <- [0 .. 2]]
      in P.PixelRGB8 r g b

toImage
  :: (R.Source r Vec3)
  => Array r DIM2 Vec3 -> P.Image P.PixelRGB8
toImage canvas = P.generateImage fromCoords Params.imgHeight Params.imgWidth
  where
    fromCoords i j = convertToPixel . (round <$>) . ((255 *) <$>) $ getColor i j
      where
        getColor i j = canvas ! (Z :. i :. j)
        convertToPixel (Triple r g b) = P.PixelRGB8 r g b

imageToText :: P.Image P.PixelRGB8 -> TL.Text
imageToText =
  TL.fromStrict .
  Data.Text.Encoding.decodeUtf8 .
  Data.ByteString.Base64.encode .
  Data.ByteString.Lazy.Char8.toStrict . P.encodePng
