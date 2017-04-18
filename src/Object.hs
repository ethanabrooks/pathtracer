{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}

module Object where

import qualified Data.Array.Repa     as R -- for Repa
import Data.Array.Repa (Array, DIM1, DIM2, U, D, Z (..), (:.)(..), (!), (++), (*^), (+^))
import Data.Array.Repa.Repr.Unboxed (Unbox)
import Data.Vector.Unboxed (Vector, MVector)
import qualified Data.Vector.Primitive as P
import Data.Array.Repa.Eval (Elt)
-- import Data.Vector.Unboxed (Vector)
import Lib (Vec3, Vec1, Rays (..), _num, _origins, _distances, inf, lessThan, filterWith)
import Debug.Trace
import Data.Maybe

data Form = Disk
            { _center :: Vec1
            , _normal :: Vec1 }
          | InfinitePlane
            { _normal :: Vec1 }
 
data Object = Object { _color :: Vec1
                     , _light :: Array D DIM1 Bool
                     , _form  :: Form }


infPlane = Object 
  { _color = R.fromFunction (Z :. 3) $ const 1
  , _light = R.fromFunction (Z :. 3) $ const False
  , _form  = InfinitePlane
             { _normal = R.fromFunction (Z :. 3) $ const 1 } }

distanceFrom :: Rays -> Object -> Vec1
distanceFrom rays object = R.fromFunction (Z :. 0) $ const 0 -- TODO


intersectWith :: Rays -> Vec3 -> Vec3
rays `intersectWith` points = points -- TODO

reflect :: Vec3 -> Vec3 -> Vec3
normals `reflect` vectors = vectors

filter3 ::  (Unbox a, Monad m) =>
  Array D DIM1 (Maybe Int) -> (Maybe Int -> Bool) -> Array D DIM2 a -> m (Array D DIM2 a)
filter3 filterer cond filtered = do
  let (Z :. len)       = R.extent filterer
  let filteredGetter n = filtered ! (Z :. n `quot` 3 :. n `mod` 3)
  flattened <- R.selectP (cond . (R.linearIndex filterer)) filteredGetter len
  return $ R.reshape (Z :. len :. 3) flattened


filter' filterer

filter1 :: (Unbox a, R.Source r a, Monad m) =>
    Array D DIM1 (Maybe Int) -> (Maybe Int -> Bool) -> Array r DIM1 a -> m (Array U DIM1 a)
filter1 filterer cond array = R.selectP (cond . (R.linearIndex filterer)) (R.linearIndex array) len
  where (Z :. len) = R.extent array

-- terminal :: (Unbox a, R.Source r a, Monad m) => Array r DIM1 a -> m (Array U DIM1 a)
-- terminal = filter' isJust 



-- f = do let d1 = R.fromFunction (Z :. 10) $ \(Z :. i) -> i :: Int
--        let closestObjectIdxs = R.fromFunction (Z :. 10) $ \(Z :. i) -> Just i :: Maybe Int
--        let nonterminal1 = filter1 closestObjectIdxs isJust
--        let [terminal, nonTerminal] = map (filter1 closestObjectIdxs) [isNothing, isJust]
--        x <- terminal d1 :: Maybe (Array U DIM1 Int)
--        return d1

broadcast :: (Unbox a, R.Source r a, R.Source s a) =>
    (a -> a -> a) -> Array r DIM1 a -> Array s DIM2 a -> Array D DIM2 a
broadcast f a1 aN = R.traverse aN id $ \getter idx@(Z :. i :. _) -> let aNVal = getter idx
                                                                        a1Val = a1 ! (Z :. i)
                                                                        in f a1Val aNVal

advancedListIndexer :: (Unbox a, R.Source r a, R.Source s (Maybe Int)) =>
    [Array r DIM1 a] -> Array s DIM1 (Maybe Int) -> Array D DIM1 a
advancedListIndexer list indexes = R.traverse indexes id
  $ \getter idx -> list !! (f $ getter idx) ! idx

distancesToObjects rays = map (distanceFrom rays) objects



bounce :: (Rays, Array D DIM1 (Array D DIM1 Float)) -> (Rays, Array D DIM1 (Array D DIM1 Float))
bounce (rays, canvas) = fromJust $ do
        -- get values for new rays
        let Rays { _origins   = origins
                 , _vectors   = vectors
                 , _distances = distances
                 , _pixels    = pixels 
                 , _num       = num } = rays

        -- get distances to closest objects
        let distancesToObjects  = map (distanceFrom rays) objects  :: [Vec1]
        let closestObjectIdxs   = minIndex distancesToObjects      :: Array D DIM1 (Maybe Int)
        let getDistance get i   = maybe inf (\j -> distancesToObjects !! j ! i) $ get i  
        let closestDistances    = R.traverse closestObjectIdxs id getDistance :: Vec1
        let nonterminal3        = filter3 closestObjectIdxs isJust 
        let [terminal, nonterminal] = map (filter1 closestObjectIdxs) [isNothing, isJust]
        let closestObjects      = R.selectP (isJust . R.linearIndex closestObjectIdxs)
                                  (\n -> objects !! closestObjectIdxs ! (Z :. n)) num
        
        distances' <- nonterminal distances
        pixels'    <- nonterminal pixels
        origins'   <- nonterminal3 origins -- TODO
  
        let (Z :. num') = R.extent distances'

        -- let nonterminalOrigins = filterWith closestObjectIdxs isJust origins
        let points   = origins' +^ broadcast (*) distances' vectors
        -- let normals  = 
        let vectors' = normals `reflect` vectors

        let rays' = Rays { _origins   = rays `intersectWith` points                  :: Vec3
                         , _distances = distances'
                         , _pixels    = pixels'
                         , _vectors   = vectors -- TODO
                         , _num       = num' }

        -- for new canvas
        let canvas' = R.traverse closestObjectIdxs id (\f i -> let objectColor = getColor $ f i
                                                               in (canvas ! i) *^ objectColor)
        return (rays', canvas')
  
mapRows ::
  (Elt a, Unbox a, Elt b, Unbox b) =>
  Int -> (a -> (Array D DIM1 b)) -> Array D DIM1 a -> Array D DIM2 b
mapRows w f array = R.fromFunction (Z :. h :. w) (\(Z :. i :. j) -> rows ! (Z :. i) ! (Z :. j))
         where (Z :. h) = R.extent array 
               rows = R.map f array

dvec = R.fromFunction (Z :. 2) (\(Z :. i) -> R.fromFunction (Z :. 3) (\(Z :. j) -> i + j))
dobj = R.fromFunction (Z :. 2) $ const infPlane
                        
intersectionWith :: Rays -> Object -> Vec1
rays `intersectionWith` object = R.fromFunction (Z :. 3) $ const 0 --TODO

origins' :: Array D DIM1 (Array D DIM1 Int)
origins' = R.fromFunction (Z :. 10) $ \(Z :. x) -> R.fromFunction (Z :. 3) $ \(Z :. y) -> y
          

getColor :: Maybe Int -> Array D DIM1 Float
getColor i = R.fromFunction (Z :. 3) $ const 0--_color $ objects !! i


-- TODO lights?

objects = [infPlane]

-- distancesFrom :: Rays -> Array D DIM2 Float
-- distancesFrom rays = foldl (R.append) empty $ map (distanceFrom rays) objects
--   where empty = R.fromFunction (Z :. numRays :. 0) $ const 0

-- distances' :: Rays -> Maybe (Array U DIM1 Float)
-- distances' rays = R.foldP min inf (distancesFrom rays)

constant :: R.Shape sh => sh -> Float -> Array D sh Float
constant sh val = R.fromFunction sh $ const val

zeros :: R.Shape sh => sh -> Array D sh Float
zeros sh = constant sh 0

zeros' = zeros (Z :. 10) :: Vec1

minIndexOf2 :: (Vec1, Array D DIM1 (Maybe Int)) -> (Vec1, Int) -> (Vec1, Array D DIM1 (Maybe Int))
minIndexOf2 (minVal, minIdx) (val, idx) = (R.zipWith min minVal val, minIdx')
  where minIdx' = R.fromFunction (R.extent minVal) (\sh -> if (val `lessThan` minVal ! sh)
                                                           then Just idx
                                                           else minIdx ! sh)

minIndex :: [Vec1] -> Array D DIM1 (Maybe Int)
minIndex arrays = minIdx
  where (_, minIdx) = foldl minIndexOf2 (infs, nothings) $ zip arrays [0..]
        infs        = R.fromFunction shape $ const inf
        nothings    = R.fromFunction shape $ const Nothing
        shape       = (Z :. round inf)
        


-- closestObjectIdxs rays = minIndex $ map (distanceFrom rays) objects
  -- case arrays of
  --                   []        -> fail "Doesn't work on empty list"
  --                   head:tail -> let zeros = R.fromFunction (R.extent a) $ const 0
  --                                    (_, minIdx) = foldl minIndexOf2 (head, zeros) $ zip tail [1..]
  --                                in  return minIdx

-- f :: Monad m => m (Int)
-- f = do
--   x:xs <- []
--   return x
  
-- closestObjectPtrs rays = R.foldP maxIdx -1 (distancesFrom rays)
--   where maxIdx idx1 idx2 = case (idx1, idx2) of
-- minIndex :: (R.Shape sh, Ord e) => [Array D sh e] -> Maybe (Array D sh e)
-- minIndex arrays =
--   case arrays of
--     []   -> Nothing
--     a:as -> let zero = R.fromFunction (R.extent a) $ const 0
--                 (_, minj) = foldl (\(minSoFar, minj) ((a', j) :: Array D DIM1 Bool, Int) ->
--                                         (R.zipWith min minSoFar a,
--                                          R.fromFunction (R.extent a)
--                                          (\(Z :. i) ->
--                                             if (a' < minSoFar) ! i
--                                             then j
--                                             else minj))) ((a, zero) :: (Array D DIM1 Bool, Array D DIM1 Float)) $ zip as [1..]
--             in Just minj



--   $ \(Z :. x :. y) -> distanceFrom (objects !! y) 
--   where 
-- closest objects = R.foldP max

-- closest objects = R.foldP max $ R.map 
          
            
