{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE FlexibleContexts #-}

module Main ( main
            , RGB8
            ) where

import qualified Codec.Picture.Types          as M
import qualified Data.Array.Repa              as R 
import qualified Data.Array.Repa.Shape        as S
import qualified Data.Array.Repa.Repr.Unboxed as U
import qualified Codec.Picture                as P

import Data.Array.Repa (Array, DIM1, DIM2, U, D, Z (..), (:.)(..), (!))
import System.Environment (getArgs)
import System.FilePath (replaceExtension)
import Object (Object (..), Form (..), distanceFrom, objects, march, getNormal)
import Triple (Triple (..), RGB8, Vec3, normalize, tripleToTuple, tripleToList)
import Lib 
import Control.Monad
import Control.Monad.Loops
import Debug.Trace
import System.Random


toImage :: (R.Source r RGB8, S.Shape sh) => Array r sh RGB8 -> P.DynamicImage
toImage canvas = P.ImageRGB8 $ P.generateImage fromCoords imgHeight imgWidth
  where fromCoords i j = convert $ canvas' ! (Z :. i :. j)
        convert (Triple r g b) = P.PixelRGB8 r g b
        canvas' = reshape [imgHeight, imgWidth] canvas


main :: IO ()
main = (P.savePngImage "image.png" . toImage) canvas
    where (_, canvas) = until ((== numIters) . fst) 
            (\(n, canvas) -> let gens = randomGens (imgHeight * imgWidth) n
                             in  (n + 1, rZipWith3 rayTrace gens raysFromCam canvas))
            (1, flatten blankCanvas)
