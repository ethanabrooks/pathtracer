{-# LANGUAGE TemplateHaskell #-}
module Lib ( Ray (..)
           , reshape
           , expandDim
           , flatten
           , mapIndex
           , toSphericalCoords
           , fromSphericalCoords
           , black
           , white
           ) where

import qualified Data.Array.Repa     as R -- for Repa
import qualified Data.Array.Repa.Shape as S
import Triple (Triple (..), Vec3, RGB8, normalize)
import Data.Array.Repa (Array, DIM1, DIM2, D)

data Ray = Ray { _origin   :: Vec3
               , _vector   :: Vec3 }

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

black = pure 0 :: RGB8
white = pure 1 :: RGB8

toSphericalCoords :: Vec3 -> (Double, Double)
toSphericalCoords coord = (theta, phi)
  where [theta, phi]  = map acos [x', z]
        Triple x y z  = normalize coord
        Triple x' _ _ = normalize $ Triple x y 0

fromSphericalCoords :: Floating t => t -> t -> Triple t
fromSphericalCoords theta phi = Triple x y z
  where x = sin phi * cos theta
        y = sin phi * sin theta
        z = cos phi
