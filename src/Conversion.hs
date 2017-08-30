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
import Triple (Triple(..), Vec3, normalize, dot, tripleToList)
import Util (reshape)

listToPixelRGB8 :: [Double] -> P.PixelRGB8
listToPixelRGB8 list = P.PixelRGB8 r g b
  where
    [r, g, b] = round <$> (255 *) <$> list

repa3ToImage
  :: (R.Source r Double)
  => Array r DIM3 Double -> P.Image P.PixelRGB8
repa3ToImage canvas =
  P.generateImage fromCoords Params.height Params.width
  where
    fromCoords i j =
      listToPixelRGB8 [canvas ! (Z :. i :. j :. k) | k <- [0 .. 2]]

repa2ToImage
  :: (R.Source r Vec3)
  => Array r DIM2 Vec3 -> P.Image P.PixelRGB8
repa2ToImage canvas =
  P.generateImage fromCoords Params.height Params.width
  where
    fromCoords i j = listToPixelRGB8 . tripleToList $ canvas ! (Z :. i :. j)

imageToText :: P.Image P.PixelRGB8 -> TL.Text
imageToText =
  TL.fromStrict .
  Data.Text.Encoding.decodeUtf8 .
  Data.ByteString.Base64.encode .
  Data.ByteString.Lazy.Char8.toStrict . P.encodePng

repa1ToText
  :: (R.Source r Vec3)
  => Array r DIM1 Vec3 -> TL.Text
repa1ToText =
  imageToText . repa2ToImage . (reshape [Params.height, Params.width])
