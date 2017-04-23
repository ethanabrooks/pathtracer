{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}

module Object where

import qualified Data.Array.Repa     as R -- for Repa
import qualified Data.Array.Repa.Shape as S
import qualified Data.Array.Repa.Algorithms.Matrix as M
import qualified Data.Array.Repa.Repr.Unboxed as U

import Data.Array.Repa (Array, DIM1, DIM2, DIM3, U, D, Z (..), (:.)(..),
                        (!), (++), (*^), (+^), (-^), (/^))
import Data.Array.Repa.Slice (Any (..), All (..))
import Lib (Vec3, Vec1, Rays (..), _num, _origins, _distances, inf, lessThan, filterWith, emptyRays)
import Debug.Trace
import Data.Maybe

data Form = Disk
            { _center :: Vec1
            , _normal :: Vec1 }
          | InfinitePlane
            { _normal :: Vec1
            , _point  :: Vec1 }
 
data Object = Object { _color :: Vec1
                     , _light :: Bool
                     , _form  :: Form }

mmMult  :: (Monad m, R.Source r Float)
     => Array r DIM2 Float
     -> Array r DIM2 Float
     -> m (Array U DIM2 Float)
mmMult a b = R.sumP (R.zipWith (*) aRepl bRepl)
    where
      t     = R.transpose b
      aRepl = R.extend (Z :. All :.colsB :.All) a
      bRepl = R.extend (Z :.rowsA :.All :.All) t
      (Z :.colsA :.rowsA) = R.extent a
      (Z :.colsB :.rowsB) = R.extent b

infPlane = Object 
  { _color = R.fromFunction (Z :. 3) $ const 1
  , _light = False
  , _form  = InfinitePlane
             { _normal = R.fromFunction (Z :. 3) $ const 1 
             , _point  = R.fromFunction (Z :. 3) $ const 3 } }

objects :: [Object]
objects = [infPlane]

distanceFrom' :: Monad m => Rays -> Form -> m (Array D DIM2 Float)
distanceFrom' rays@Rays { _origins = origins, _vectors = vectors } form =
  case form of
    Disk { _center = center, _normal = normal }        ->
      distanceFrom' rays $ InfinitePlane { _point  = center , _normal = normal }
    InfinitePlane { _point = point, _normal = normal } -> 
      do broadcasted         <- broadcast point origins
         let transposeNormal = R.reshape (Z :. 3 :. 1) normal
         a                   <- (broadcasted -^ origins) `mmMult` transposeNormal 
         b                   <- _vectors rays `mmMult` transposeNormal
         return $ a /^ b

distanceFrom :: Monad m => Rays -> Object -> m Vec1
distanceFrom rays object = do distances <- distanceFrom' rays $ _form object
                              let flatten = R.reshape (Z :. (S.size $ R.extent distances)) distances
                              return flatten 
                              

broadcast 
  :: (R.Source r e, R.Source s e, Monad m) => Array r DIM1 e
     -> Array s DIM2 e
     -> m (Array D (R.FullShape (Any DIM1 :. Int)) e)
broadcast array like
    | len == h  = return               $ R.extend (Any :. (w :: Int)) array
    | len == w  = return . R.transpose $ R.extend (Any :. (h :: Int)) array
    | otherwise = fail "`array` must have at least one dimension equal to `like`"
    where (Z :. len) = R.extent array
          (Z :. h :. w) = R.extent like

                               
-- broadcast :: (U.Unbox a, R.Source r a, R.Source s a) =>
--     Array r DIM1 a -> (a -> a -> a) -> Array s DIM2 a -> Array D DIM2 a
-- broadcast a1 op aN = R.traverse aN id $ \getter idx@(Z :. i :. _) -> let aNVal = getter idx
--                                                                          a1Val = a1 ! (Z :. i)
--                                                                       in a1Val `op` aNVal
origins :: Array D DIM2 Float
origins = R.fromFunction (Z :. 10 :. 3) $ \(Z :. i :. j) -> fromIntegral $ i + j

point :: Array D DIM1 Float
point = R.fromFunction (Z :. 3) $ \(Z :. i) -> fromIntegral i


march :: Monad m => Rays -> Array U DIM1 Float -> m Vec3
march Rays { _origins = origins, _vectors = vectors } distances =
  do broadcasted <- broadcast distances vectors
     return $ origins +^ (broadcasted *^ vectors)

getNormal :: Object -> Vec1
getNormal Object { _form = form } = _normal form

reflect :: Vec3 -> Vec3 -> Vec3
normals `reflect` vectors = vectors

norm2 ::
  (Floating b, R.Source r b, U.Unbox b, R.Shape sh, Monad m) =>
  Array r (sh :. Int) b -> m (Array D sh b)
norm2 vector = (R.sumP $ R.map (**2) vector) >>= return . (R.map sqrt)

normalize :: Vec3 -> Vec3
normalize vector = vector -- TODO

v1 :: Array U DIM1 Int
v1 = R.computeS $ R.fromFunction (Z :. 3) $ \(Z :. i) -> (i :: Int)
  

v2 :: Array U DIM2 Int
v2 = R.computeS $ R.fromFunction (Z :. 2 :. 3) $ \(Z :. i :. j) -> (2 * i + j :: Int)


-- toSpericalCoords :: Vec3 -> (Vec1, Vec1)
-- toSpericalCoords cartesianCoords =
--   acos 


filter3 ::  (U.Unbox a, Monad m) =>
  Array D DIM1 b -> (b -> Bool) -> Array D DIM2 a -> m (Array D DIM2 a)
filter3 filterer cond filtered = do
  let (Z :. len)       = R.extent filterer
  let filteredGetter n = filtered ! (Z :. n `quot` 3 :. n `mod` 3)
  flattened <- R.selectP (cond . (R.linearIndex filterer)) filteredGetter len
  return $ R.reshape (Z :. len :. 3) flattened


filter1 :: (U.Unbox a, R.Source r a, Monad m) =>
    Array D DIM1 b -> (b -> Bool) -> Array r DIM1 a -> m (Array U DIM1 a)
filter1 filterer cond array = R.selectP (cond . (R.linearIndex filterer)) (R.linearIndex array) len
  where (Z :. len) = R.extent array


catMaybeArray :: (U.Unbox a, Monad m) => Array D DIM1 (Maybe a) -> m (Array U DIM1 a)
catMaybeArray array = R.selectP (isJust . (R.linearIndex array))
                      (fromJust . (R.linearIndex array)) . S.size $ R.extent array

unnestArray array w = R.fromFunction (Z :. h :. w) $ \(Z :. i :. j) -> array ! (Z :. i) ! (Z :. j)
  where (Z :. h) = R.extent array

                           

bounce :: (Rays, Array D DIM1 (Array D DIM1 Float)) -> (Rays, Array D DIM1 (Array D DIM1 Float))
bounce (rays, canvas) = fromJust $ do
        -- get values for new rays
        let Rays { _origins   = origins
                 , _vectors   = vectors
                 , _distances = distances
                 , _pixels    = pixels 
                 , _num       = num } = rays

        -- get distances to closest objects
        let distancesToObjects   = mapMaybe (distanceFrom rays) objects  :: [Vec1]
        let closestObjectIdxs    = minIndex distancesToObjects      :: Array D DIM1 (Maybe Int)
        let getDistance getIdx i = maybe inf (\j -> distancesToObjects !! j ! i) $ getIdx i  
        let closestDistances     = R.traverse closestObjectIdxs id getDistance

        -- get filter criteria
        let checkLight getIdx i  = maybe False (_light . (objects !!)) $ getIdx i
        let hitLight             = R.traverse closestObjectIdxs id checkLight
        let hitNothing           = R.map isNothing closestObjectIdxs
        let terminal             = R.zipWith (||) hitLight hitNothing
        let nonterminal          = R.map not terminal
        
        -- filter out terminal
        distances'          <- filter1 nonterminal id closestDistances
        pixels'             <- filter1 nonterminal id pixels
        origins'            <- filter3 nonterminal id origins
        closestObjectIdxs'  <- catMaybeArray closestObjectIdxs

        -- get new vectors
        points            <- march rays distances'
        let nestedNormals = R.map (getNormal . (objects !!)) closestObjectIdxs'
        let normals       = unnestArray nestedNormals 3
        let vectors'      = normals `reflect` vectors

        let rays' = Rays { _origins   = points
                         , _distances = distances'
                         , _pixels    = pixels'
                         , _vectors   = vectors'
                         , _num       = S.size $ R.extent distances' }

        -- Every bounce, multiply color of objects struck by rays with the canvas
        let canvas' = R.traverse closestObjectIdxs id $ \f i ->
              let objectColor = getColor $ f i
                  pixel       = (Z :. pixels ! i)
              in (canvas ! pixel) *^ objectColor

        return (rays', canvas')
  


getColor :: Maybe Int -> Array D DIM1 Float
getColor = maybe black (\i -> _color $ objects !! i)
  where black = R.fromFunction (Z :. 3) $ const 0

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


