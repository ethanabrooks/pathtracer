{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Object where

import qualified Data.Array.Repa     as R -- for Repa
import qualified Data.Array.Repa.Shape as S
import qualified Data.Array.Repa.Algorithms.Matrix as M
import qualified Data.Array.Repa.Repr.Unboxed as U

import Data.Array.Repa (Array, DIM1, DIM2, DIM3, U, D, Z (..), (:.)(..),
                        (!), (++), (*^), (+^), (-^), (/^))
import Data.Array.Repa.Slice (Any (..), All (..))
import Lib (Vec3, Vec1, Rays (..), _num, _origins, _distances, inf,
            lessThan, filterWith, emptyRays, reshape, expandDim)
import Debug.Trace
import qualified Data.Vector as V
import Data.Maybe
import Control.Monad
import Prelude hiding ((++))

data Form = Disk
            { _center :: Vec1
            , _normal :: Vec1
            , _radius :: Double }
          | InfinitePlane
            { _normal :: Vec1
            , _point  :: Vec1 }
 
data Object = Object { _color      :: Vec1
                     , _light      :: Bool
                     , _reflective :: Bool
                     , _form       :: Form }

infPlane = Object 
  { _color      = R.fromFunction (Z :. 3) $ const 1
  , _light      = False
  , _reflective = True
  , _form       = InfinitePlane
             { _normal = R.fromFunction (Z :. 3) $ const 1 
             , _point  = R.fromFunction (Z :. 3) $ const 3 } }

objects :: V.Vector Object
objects = V.fromList [infPlane] 

countsAsHit :: Double -> Bool
countsAsHit distance = distance >= 0 && distance < inf

bounce :: (Rays, Array D DIM1 (Array D DIM1 Double)) -> (Rays, Array D DIM1 (Array D DIM1 Double))
bounce (rays, canvas) = fromJust $ do

        -- get values for new rays
        let Rays { _origins   = origins
                 , _vectors   = vectors
                 , _distances = distances
                 , _pixels    = pixels 
                 , _num       = num } = rays

        distsToObjects <- distancesFrom rays
        let closestObjectIdxs   = R.map V.minIndex distsToObjects
              :: Array D DIM1 Int
        let closestDistances    = R.zipWith (V.!) distsToObjects closestObjectIdxs
              :: Array D DIM1 Double
        let closestObjectIdxs'  = R.zipWith (\d i -> if countsAsHit d then Just i else Nothing)
                                    closestDistances closestObjectIdxs
              :: Array D DIM1 (Maybe Int)
        let closestObjects    = R.map (fmap (objects V.!)) closestObjectIdxs'
              :: Array D DIM1 (Maybe Object)

        -- get filter criteria
        let hitLight           = R.map checkLight closestObjects    :: Array D DIM1 Bool
        let hitNothing         = R.map isNothing closestObjects     :: Array D DIM1 Bool
        let terminal           = R.zipWith (||) hitLight hitNothing :: Array D DIM1 Bool
        let nonterminal        = R.map not terminal                 :: Array D DIM1 Bool
        
        -- filter out terminal
        distances'            <- filter' nonterminal closestDistances
        pixels'               <- filter' nonterminal pixels
        closestObjectIdxs'    <- filter' nonterminal closestObjectIdxs

        -- get new values for rays
        let closestObjects'    = R.map (objects V.!) closestObjectIdxs'
        vectors'              <- closestObjects' `reflect` vectors
        origins'              <- march rays distances'

        let rays' = Rays { _origins   = origins'
                         , _distances = distances'
                         , _pixels    = pixels'
                         , _vectors   = vectors'
                         , _num       = S.size $ R.extent distances' }

        -- Every bounce, multiply color of objects struck by rays with the canvas
        let canvas' = R.zipWith (applyColor canvas) closestObjects pixels

        return (rays', canvas')

getNormal :: Object -> Vec1
getNormal Object { _form = form } = _normal form

norm2 ::
  (Floating b, R.Source r b, U.Unbox b, R.Shape sh, Monad m) =>
  Array r (sh :. Int) b -> m (Array D sh b)
norm2 vector = (R.sumP $ R.map (**2) vector) >>= return . (R.map sqrt)

normalize ::
  (Ord c, U.Unbox c, R.Source r1 c, Floating c, Monad m) =>
  Array r1 (DIM1 :. Int) c -> m (Array D (DIM1 :. Int) c)
normalize vector = do norm        <- norm2 vector
                      let length   = R.map (max 0.0000001) norm
                      broadcasted <- broadcast length vector
                      return $ vector /^ broadcasted

reflect :: (R.Source r Double, Monad m) =>
   Array D DIM1 Object -> Array r DIM2 Double -> m (Array D DIM2 Double)
objects `reflect` vectors =
  do let normals   = getNormals objects
     vectorAngles <- toSphericalCoords (R.map negate vectors)
     normalAngles <- toSphericalCoords normals
     let [newTheta, newPhi] = [vecAngle +^ R.map (*2) (normAngle -^ vecAngle)
                              | (vecAngle, normAngle) <- zip vectorAngles normalAngles]
     return $ fromSphericalCoords newTheta newPhi
  -- TODO : random reflection

distancesFrom :: Monad m => Rays -> m (Array D DIM1 (V.Vector Double))
distancesFrom rays = V.mapM (distanceFrom rays) objects >>= return . invert
  
distanceFrom :: Monad m => Rays -> Object -> m (Array D DIM1 Double)
distanceFrom rays object = distanceFrom' rays (_form object)

-- | Gets the distance from the origin of rach ray, along the vector of the ray, to the point
--   where the ray would strike the object, or if it would miss the object, returns inf.
distanceFrom' :: Monad m => Rays -> Form -> m (Array D DIM1 Double)
distanceFrom' rays@Rays { _origins = origins, _vectors = vectors } form =
  case form of
    Disk { _center = center, _normal = normal, _radius = radius }  -> do
      let infPlane    = InfinitePlane { _point  = center , _normal = normal }
      distsFromOrigin <- distanceFrom' rays infPlane 
      points          <- march rays distsFromOrigin
      center'         <- broadcast center points
      distsFromCenter <- norm2 $ points -^ center'
      return $ R.zipWith
        (\dFromCenter dFromOrigin -> if dFromCenter < radius then dFromOrigin else inf)
        distsFromCenter distsFromOrigin
    InfinitePlane { _point = point, _normal = normal } -> 
      do points'         <- broadcast point origins
         transposeNormal <- R.computeP $ R.reshape (Z :. 3 :. 1) normal
         originToPoint   <- R.computeP $ points' -^ origins
         vectors         <- R.computeP $ _vectors rays
         a               <- originToPoint `M.mmultP` transposeNormal
         b               <- vectors `M.mmultP` transposeNormal     
         return $ reshape [-1] $ a /^ b

march ::
  (R.Source r Double, Monad m) =>
  Rays -> Array r DIM1 Double -> m (Array D DIM2 Double)
march Rays { _origins = origins, _vectors = vectors } distances =
  do broadcasted <- broadcast distances vectors
     return $ origins +^ (broadcasted *^ vectors)


broadcast 
  :: (R.Source r e, R.Source s e, Monad m) => Array r DIM1 e
     -> Array s DIM2 e
     -> m (Array D (R.FullShape (Any DIM1 :. Int)) e)
broadcast array like
    | len == h  = return               $ R.extend (Any :. (w :: Int)) array
    | len == w  = return . R.transpose $ R.extend (Any :. (h :: Int)) array
    | otherwise = fail "`array` must have at least one dimension equal to `like`"
    where (Z :. len)    = R.extent array
          (Z :. h :. w) = R.extent like

                               

toSphericalCoords ::
  (Floating b, U.Unbox b, Ord b, Monad m, R.Source r b) =>
  Array r ((Z :. Int) :. Int) b -> m [Array D DIM1 b]
toSphericalCoords cartesianCoords =
  do let (Z :. h :. _) = R.extent cartesianCoords
     let xyComponent   = R.fromFunction (Z :. h :. 2) (cartesianCoords !)
     normalized        <- normalize xyComponent
     let xComponent    = R.slice normalized      (Any :. (0 :: Int))
     let zComponent    = R.slice cartesianCoords (Any :. (2 :: Int))
     return $ map (R.map acos) [xComponent, zComponent]

fromSphericalCoords ::
  (R.Source r1 e, R.Source r e, Floating e) =>
  Array r1 DIM1 e
  -> Array r DIM1 e -> Array D DIM2 e
fromSphericalCoords theta phi = x' ++ y' ++ z'
  where x = R.map sin phi *^ R.map cos theta
        y = R.map sin phi *^ R.map sin theta
        z = R.map cos phi
        [x', y', z'] = map (expandDim 1) [x, y, z]


unnestArray :: (R.Source r (Array s DIM1 a), R.Source s a) =>
    Int -> Array r DIM1 (Array s DIM1 a) -> Array D DIM2 a
unnestArray w array = R.fromFunction (Z :. h :. w) $ \(Z :. i :. j) -> array ! (Z :. i) ! (Z :. j)
  where (Z :. h) = R.extent array


invert :: (R.Source s a, S.Shape sh) => V.Vector (Array s sh a) -> Array D sh (V.Vector a)
invert vec | V.null vec = error "cannot invert empty vector."
           | not valid  = error "Not all shapes of arrays in list are equal."
           | valid      = foldl (R.zipWith V.snoc) empty vec 
  where shape = R.extent $ V.head vec
        empty = R.fromFunction shape $ const V.empty
        valid = all ((== shape) . R.extent) vec

getNormals :: Array D DIM1 Object -> Array D DIM2 Double
getNormals = unnestArray 3 . R.map getNormal

checkLight :: Maybe Object -> Bool
checkLight = maybe False _light
                         
filter' :: (U.Unbox a, Monad m, R.Source r a, R.Source s Bool, S.Shape sh1, S.Shape sh2) =>
    Array s sh1 Bool -> Array r sh2 a -> m (Array U DIM1 a)
filterer `filter'` filtered = R.selectP cond constructor sizeFiltered
        where cond              = R.linearIndex filterer . (`quot` width)
              constructor       = R.linearIndex filtered
              sizeFiltered      = S.size $ R.extent filtered
              width             = sizeFiltered `quot` (S.size $ R.extent filterer)

applyColor ::
  (R.Source r1 Double, R.Source r (Array r1 (Z :. Int) Double),
   S.Shape (Z :. head)) =>
  Array r (Z :. head) (Array r1 (Z :. Int) Double)
  -> Maybe Object -> head -> Array D (Z :. Int) Double
applyColor canvas object pixelIdx = canvasColor *^ objectColor
  where canvasColor = canvas ! (Z :. pixelIdx)
        objectColor = maybe black _color object
        black       = R.fromFunction (Z :. 3) $ const 0
  
origins :: Array D DIM2 Double
origins = R.fromFunction (Z :. 10 :. 3) $ \(Z :. i :. j) -> fromIntegral $ i + j

point :: Array D DIM1 Double
point = R.fromFunction (Z :. 3) $ \(Z :. i) -> fromIntegral i


v1 :: Array U DIM1 Double
v1 = R.computeS $ R.fromFunction (Z :. 6) $ \(Z :. i) -> fromIntegral i
  

v2 :: Array U DIM2 Double
v2 = R.computeS $ R.fromFunction (Z :. 2 :. 3) $ \(Z :. i :. j) -> fromIntegral $ 2 * i + j


(~=) :: (Num a, Ord a, Fractional a) => a -> a -> Bool
a ~= b = (a - b) * (a - b) < 0.00001

(~=^) :: (R.Source r1 a, R.Source r2 a, R.Shape sh, Ord a, Num a, Fractional a) =>
  Array r1 sh a -> Array r2 sh a -> Bool
a ~=^ b = R.foldAllS (&&) True $ R.zipWith (~=) a b

(~=.) :: (R.Source r1 a, R.Shape sh, Ord a, Num a, Fractional a) =>
  Array r1 sh a -> a -> Bool
a ~=. x = R.foldAllS (&&) True $ R.map (~= x) a
