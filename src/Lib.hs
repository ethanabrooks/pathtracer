{-# LANGUAGE TemplateHaskell #-}
module Lib ( Ray (..)
           , Canvas
           , inf
           , reshape
           , expandDim
           , flatten
           , mapIndex
           ) where

import qualified Data.Array.Repa     as R -- for Repa
import qualified Data.Array.Repa.Shape as S
import Triple (Vec3, RGB8)
import Data.Array.Repa (Array, DIM1, DIM2, D)

type Canvas = Array D DIM2 RGB8
data Ray = Ray { _origin   :: Vec3
               , _vector   :: Vec3 }

inf :: Double
inf = 1 / 0

inferMissing :: (Show a, Integral a) => [a] -> [a] -> [a]
inferMissing list listWithNeg
  | not valid = error ((show list) ++ " and " ++ (show listWithNeg) ++ " are not valid inputs.")
  | valid     = result
    where valid      = (product result == product list) && (all (> 0) result)
          missingVal = product list `quot` product (filter (>= 0) listWithNeg)
          result     = map (\x -> if x < 0 then missingVal else x) listWithNeg

mapIndex ::
  (S.Shape sh', R.Source r a) =>
  (sh' -> b) -> Array r sh' a -> Array D sh' b
mapIndex f array = R.traverse array id $ const f


reshape :: (R.Source r1 e, S.Shape sh1, S.Shape sh2) => [Int] -> Array r1 sh1 e -> Array D sh2 e
reshape shape array = R.reshape (S.shapeOfList shape') array
  where shape' = inferMissing (S.listOfShape (R.extent array)) shape

flatten :: (R.Source r1 e, S.Shape sh1, S.Shape sh2) => Array r1 sh1 e -> Array D sh2 e
flatten array = reshape [-1] array

expandDim :: (R.Source r1 e, S.Shape sh1, S.Shape sh2) => Int -> Array r1 sh1 e -> Array D sh2 e
expandDim dim array = R.reshape shape array
  where shape            = S.shapeOfList . (insertAt dim 1) . S.listOfShape $ R.extent array

insertAt :: Int -> a -> [a] -> [a]
insertAt n x list = (take n list) ++ [x] ++ (drop n list)
