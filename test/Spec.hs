{-# LANGUAGE FlexibleInstances, TypeOperators, TemplateHaskell #-}

import Test.QuickCheck.Monadic
import Data.List (intersperse)
import Object (norm2, normalize)

import Test.QuickCheck (quickCheckAll)
import qualified Test.QuickCheck as T
import qualified Data.Array.Repa     as R -- for Repa
import qualified Data.Array.Repa.Shape as S
import qualified Data.Array.Repa.Algorithms.Matrix as M
import qualified Data.Array.Repa.Repr.Unboxed as U
import qualified Data.Array.Repa.Arbitrary as A
import Data.Array.Repa (Array, DIM1, DIM2, DIM3, U, D, Z (..), (:.)(..),
                        (!), (++), (*^), (+^), (-^), (/^))


mkVector :: U.Unbox a => [a] -> Array D DIM1 a
mkVector list = R.delay $ R.fromListUnboxed (Z :. length list) list

mkMatrix :: U.Unbox a => [[a]] -> Array D DIM2 a
mkMatrix [] = R.delay $ R.fromListUnboxed (Z :. 0 :. 0) []
mkMatrix lists@(list:_) | not valid = error "Inner lists must be equal length"
                        | valid     = R.delay $ R.fromListUnboxed (Z :. h :. w) $ concat lists
  where valid = all ((== w) . length) lists
        h     = length lists
        w     = length list


(~=) :: (Num a, Ord a, Fractional a) => a -> a -> Bool
a ~= b = (a - b) * (a - b) < 0.00001

(~=^) :: (R.Source r1 a, R.Source r2 a, R.Shape sh, Ord a, Num a, Fractional a) =>
  Array r1 sh a -> Array r2 sh a -> Bool
a ~=^ b = R.foldAllS (&&) True $ R.zipWith (~=) a b

(~=.) :: (R.Source r1 a, R.Shape sh, Ord a, Num a, Fractional a) =>
  Array r1 sh a -> a -> Bool
a ~=. x = R.foldAllS (&&) True $ R.map (~= x) a


prop_norm2 :: T.Property
prop_norm2 = monadicIO $ do
  actual <- norm2 (mkMatrix ([[1.0, 1.0],
                              [2.0, 2.0]] :: [[Double]]))
  assert $ actual ~=^ mkVector [1.4142135624, 2.8284271247]


prop_id :: Array U DIM2 Double -> T.Property
prop_id array = monadicIO $ array `R.equalsP` array >>= assert
  

prop_normalize :: Array U DIM2 Double -> T.Property
prop_normalize array = monadicIO $
  do normalized  <- normalize array
     norm2 normalized >>= assert . (~=. 1)


return []
runTests = $quickCheckAll

quickCheckArray :: (T.Arbitrary a, T.Testable prop, Show a) => (a -> prop) -> IO () 
quickCheckArray = T.quickCheck . (T.forAll (T.scale (+1) T.arbitrary))

main = do
  quickCheckArray prop_id
  quickCheckArray prop_normalize
  T.quickCheck prop_norm2
