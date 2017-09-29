#!/usr/bin/env stack
-- stack --resolver lts-8.12 script --optimize
{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE BangPatterns #-}

module Main
  ( main
  ) where

import qualified Data.Array.Repa as R
import Data.Array.Repa (Array, DIM1, (+^), U, Z(..), (:.)(..), (!))
import Data.Time
import Prelude hiding (sum)
import qualified System.Random as Random

n :: Int
n = round (1e6 :: Double)

addLargeRandomArrays
  :: Monad m
  => Array U DIM1 Int -> m (Array U DIM1 Int)
addLargeRandomArrays array = R.computeP $ array +^ array'
  where
    i = array ! (Z :. 0) -- use the first element in the array as the random seed
    array' =
      R.fromListUnboxed (Z :. n) . take n $ Random.randoms (Random.mkStdGen i) -- random array

main :: IO ()
main = do
  start <- getCurrentTime
  loop start zeros
  where
    zeros = R.computeUnboxedS $ R.fromFunction (Z :. n) $ const 0 -- all zeros
    loop prev array = do
      array' <- addLargeRandomArrays array
      sum <- (! Z) <$> R.sumP array'
      now <- getCurrentTime
      print (diffUTCTime now prev, sum)
      loop now array'
