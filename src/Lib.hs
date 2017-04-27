{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeOperators #-}
module Lib ( Rays (..)
           , Vec1
           , Vec3
           , inf
           , lessThan
           , filterWith
           , emptyRays
           , reshape
           ) where

import Control.Lens
import qualified Data.Array.Repa     as R -- for Repa
import Data.Array.Repa (Array, DIM1, DIM2, U, D, Z (..), (:.)(..), (!), (*^), All)
import Data.Array.Repa hiding ((++), map, reshape)
import Data.Array.Repa.Repr.Unboxed (Unbox)
import Data.Array.Repa.Eval (Elt)
import Codec.Picture
import qualified Data.Array.Repa.Shape as S


data FooBar
  = Foo { _x :: [Int], _y :: Int }
  | Bar { _x :: [Int] } deriving (Show)

makeLenses ''FooBar

foo = Foo { _x = [0..5], _y = 6 }
justSix = foo ^? y
x_ = foo ^. x
foo' = foo & x %~ map (+ 1)
foo'' = foo & x .~ [0..9]
foo''' = foo & x . ix 2 .~ 8

d1 :: Array D (Z :. Int) Double
d1 = R.fromFunction (Z :. 10) $ \(Z :. x) -> case x of
                                                  1 -> 1
                                                  3 -> 3
                                                  5 -> 5
                                                  _ -> 1 / 0

-- | Paramters
imgHeight = 2--1200
imgWidth  = 4--1200
cameraDepth = 10 

numRays = imgHeight * imgWidth

imgDimensions :: DIM2
imgDimensions = (Z :. imgHeight :. imgWidth)
 

d2 :: Array D (Z :. Int) Double
d2 = R.fromFunction (Z :. 10) $ \(Z :. x) -> fromIntegral x
 

d3 :: Array D (Z :. Int) Double
d3 = R.fromFunction (Z :. 5) $ \(Z :. x) -> fromIntegral x


dInf :: Array D (Z :. Int) Double
dInf = R.fromFunction (Z :. round inf) $ const 0.0

u1 :: Array U (Z :. Int) Double
u1 = R.computeUnboxedS d1

u2 :: Array U (Z :. Int) Double
u2 = R.computeUnboxedS d2

u3 :: Array U DIM2 Double
u3 = R.fromListUnboxed (Z :. 2 :. 2) [1, 2, 3, 4]

inf = 1.0 / 0.0 :: Double

rangeD1 :: Array D (Z :. Int) Int
rangeD1 = R.traverse d1 id (\_ (Z :. i) -> i)

type ImageArray = Array D DIM2 RGB8
type RGB8 = (Pixel8, Pixel8, Pixel8)
type Vec3 = Array D DIM2 Double
type Vec1 = Array D DIM1 Double
data Rays = Rays { _origins   :: Vec3
                 , _vectors   :: Vec3
                 , _distances :: Array U DIM1 Double
                 , _pixels    :: Array U DIM1 Int
                 , _num       :: Int }

makeLenses ''Rays

instance Show (Rays) where
  show Rays { _origins   = origins
            , _vectors   = vectors
            , _pixels    = pixel
            , _distances = distance
            , _num       = num } = "{ \n " ++ originsStr ++
                                  ",\n "  ++ distanceStr ++ 
                                  ",\n "  ++ show num ++ "\n}"
       where originsStr  = show $ R.computeUnboxedS origins
             distanceStr = show $ distance
              

newRGB :: Int -> Int -> Int -> RGB8
newRGB x y z = (pixel x, pixel y, pixel z)
  where pixel = fromIntegral . min 0xff

lessThan :: R.Shape sh => Array D sh Double -> Array D sh Double -> Array D sh Bool
lessThan a1 a2 = R.fromFunction (R.extent a1) (\coord -> (a1 ! coord) Prelude.< (a2 ! coord)) 

filterWith :: (Unbox b, R.Source r a, R.Source s b, Monad m) =>
    Array r DIM1 a -> (a -> Bool) -> Array s DIM1 b -> m (Array U DIM1 b)
filterWith filterer cond filtered = R.selectP (cond . from filterer) (from filtered) length
    where (Z :. length) = R.extent filtered
          from :: (R.Source r c) => Array r DIM1 c -> Int -> c
          from array n = array ! (Z :. n)

-- filterBy :: (R.Source r b, R.Source r a, Elt a, Unbox a, Monad m) =>
--   Array r DIM2 a -> (Array r DIM1 b, b -> Bool) -> m (Array U DIM1 a)
-- filtered `filterBy` (filterer, cond) = R.selectP (cond . distance') eltForNew (numRays * width)
--     where (Z :. _ :. width) = R.extent filtered
--           distance' n       = filterer ! (Z :. n `quot` width)
--           eltForNew n       = filtered ! (Z :. n `quot` width :. n `mod` width)

-- -- reshape after filtering
-- filterBy3 :: (Elt a, Unbox a) =>
--   Vec1 -> Array D DIM2 a -> (Double -> Bool) -> Maybe (Array D DIM2 a)
-- filterBy3 filterer filtered cond = do newArray <- filtered `filterBy` (filterer, cond)
--                                       let (Z :. length) = R.extent newArray
--                                       Just $ R.reshape (Z :. length `quot` 3 :. 3) newArray

-- expand dim0 before filtering
-- filterByDistance1 :: (Elt a, Unbox a) => (Double -> Bool) -> Vec1 -> Maybe (Array D DIM1 a)
-- filterBy1 :: (R.Source r b, R.Source r a, Elt a, Unbox a, Monad m) =>
--   Array D DIM1 a -> Array D DIM1 b -> (b -> Bool) -> Maybe (Array D DIM1 a)
-- filterBy1 :: (Elt a, Unbox a) =>
--     Vec1 -> Array D DIM1 a -> (Double -> Bool) -> Maybe (Array D DIM1 a)
-- filterBy1 filterer filtered cond = expanded `filterBy` (filterer, cond) >>= Just . R.delay
--         where expanded = R.reshape (Z :. length :. 1) filtered
--               (Z :. length) = R.extent filtered


rayShape3 = (Z :. imgHeight * imgWidth :. 3)
rayShape1 = (Z :. imgHeight * imgWidth)
canvas = R.fromFunction rayShape3 $ const 0 :: Vec3

rowAt :: Int -> Vec3 -> Vec3
rowAt i = R.extract (Z :. i :. 0) (Z :. 1 :. 3)



-- applyColorTo :: Rays -> Vec3 -> Array D DIM2 Double
-- applyColorTo rays canvas = R.fromFunction rayShape3 $ \(Z :. i :. j) ->
--   let [rayColor, canvasColor] = map (rowAt i) [_colors rays, canvas]
--   in (rayColor *^ canvasColor) ! (Z :. 0 :. j)

emptyRays = Rays { _origins   = empty3
                 , _vectors   = empty3
                 , _pixels    = R.computeS empty1
                 , _distances = R.fromListUnboxed  (Z :. 0) []
                 , _num       = 0 }
  where empty1 = R.fromFunction (Z :. 0) $ const 0
        empty3 = R.fromFunction (Z :. 0 :. 0) $ const 0

vec3ToImage :: Vec3 -> ImageArray
vec3ToImage array = R.reshape (Z :. imgHeight :. imgWidth)
  $ R.fromFunction (Z :. imgHeight * imgWidth) rowToRGB
  where rowToRGB (Z :. i) = let [r, g, b] = map round . R.toList $ rowAt i array
                            in  newRGB r g b


inferMissing :: (Show a, Integral a) => [a] -> [a] -> [a]
inferMissing list listWithNeg
  | not valid = error ((show list) ++ " and " ++ (show listWithNeg) ++ " are not valid inputs.")
  | valid     = result
    where valid      = (product result == product list) && (all (> 0) result)
          missingVal = product list `quot` product (filter (>= 0) listWithNeg)
          result     = map (\x -> if x < 0 then missingVal else x) listWithNeg


reshape :: (R.Source r1 e, S.Shape sh1, S.Shape sh2) => [Int] -> Array r1 sh1 e -> Array D sh2 e
reshape shape array = R.reshape (S.shapeOfList shape') array
  where shape' = inferMissing (S.listOfShape (R.extent array)) shape
