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
            lessThan, filterWith, emptyRays, reshape)
import Debug.Trace
import Data.Maybe
import Control.Monad
import Prelude hiding ((++))

data Form = Disk
            { _center :: Vec1
            , _normal :: Vec1 }
          | InfinitePlane
            { _normal :: Vec1
            , _point  :: Vec1 }
 
data Object = Object { _color :: Vec1
                     , _light :: Bool
                     , _form  :: Form }

infPlane = Object 
  { _color = R.fromFunction (Z :. 3) $ const 1
  , _light = False
  , _form  = InfinitePlane
             { _normal = R.fromFunction (Z :. 3) $ const 1 
             , _point  = R.fromFunction (Z :. 3) $ const 3 } }

objects :: [Object]
objects = [infPlane]

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

                               
march :: Monad m => Rays -> Array U DIM1 Double -> m Vec3
march Rays { _origins = origins, _vectors = vectors } distances =
  do broadcasted <- broadcast distances vectors
     return $ origins +^ (broadcasted *^ vectors)

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
                      let length  = R.map (max 0.0000001) norm
                      broadcasted <- broadcast length vector
                      return $ vector /^ broadcasted

v1 :: Array U DIM1 Double
v1 = R.computeS $ R.fromFunction (Z :. 6) $ \(Z :. i) -> fromIntegral i
  

v2 :: Array U DIM2 Double
v2 = R.computeS $ R.fromFunction (Z :. 2 :. 3) $ \(Z :. i :. j) -> fromIntegral $ 2 * i + j

reflect :: (Floating e, U.Unbox e, Ord e, R.Source r e, Monad m) =>
    Array r ((Z :. Int) :. Int) e -> Array r ((Z :. Int) :. Int) e -> m (Array D DIM2 e)
normals `reflect` vectors = --return normals
  do vectorAngles <- toSphericalCoords (R.map negate vectors)
     normalAngles <- toSphericalCoords normals
     let [newTheta, newPhi] = [vecAngle +^ R.map (*2) (normAngle -^ vecAngle)
                              | (vecAngle, normAngle) <- zip vectorAngles normalAngles]
     return $ fromSphericalCoords newTheta newPhi
  -- TODO : random reflection
  


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
        [x', y', z'] = map transposeVector [x, y, z]

transposeVector ::
  (Num head, S.Shape (Z :. head1), S.Shape ((Z :. head1) :. head),
   R.Source r1 e) =>
  Array r1 (Z :. head1) e -> Array D ((Z :. head1) :. head) e
transposeVector vector = R.reshape (Z :. len :. 1) vector
  where (Z :. len) = R.extent vector


  -- TODO: remove
catMaybeArray :: (U.Unbox a, Monad m) => Array D DIM1 (Maybe a) -> m (Array U DIM1 a)
catMaybeArray array = R.selectP (isJust . (R.linearIndex array))
                      (fromJust . (R.linearIndex array)) . S.size $ R.extent array
  -- END: remove

unnestArray :: (R.Source r (Array s DIM1 a), R.Source s a) =>
    Int -> Array r DIM1 (Array s DIM1 a) -> Array D DIM2 a
unnestArray w array = R.fromFunction (Z :. h :. w) $ \(Z :. i :. j) -> array ! (Z :. i) ! (Z :. j)
  where (Z :. h) = R.extent array


distanceFrom' :: Monad m => Rays -> Form -> m (Array D DIM2 (Maybe Double))
distanceFrom' rays@Rays { _origins = origins, _vectors = vectors } form =
  case form of
    Disk { _center = center, _normal = normal }        ->
      distanceFrom' rays $ InfinitePlane { _point  = center , _normal = normal }
    InfinitePlane { _point = point, _normal = normal } -> 
      do broadcasted     <- broadcast point origins
         transposeNormal <- R.computeP $ R.reshape (Z :. 3 :. 1) normal
         originToPoint   <- R.computeP $ broadcasted -^ origins
         vectors         <- R.computeP $ _vectors rays
         a               <- originToPoint `M.mmultP` transposeNormal
         b               <- vectors `M.mmultP` transposeNormal     
         return $ R.map (\x -> if x < 0 then Nothing else Just x) (a /^ b)

distanceFrom :: Monad m => Rays -> Object -> m (Array D DIM1 (Maybe Double))
distanceFrom rays object = do distances <- distanceFrom' rays $ _form object
                              let flatten = R.reshape (Z :. (S.size $ R.extent distances)) distances
                              return flatten 

getNormals :: R.Source r Int => Array r DIM1 Int -> Array D DIM2 Double
getNormals objectIdxs = unnestArray 3 (R.map (getNormal . (objects !!)) objectIdxs)


getDistance ::
  (R.Source r (Maybe Double), S.Shape t) =>
  [Array r t (Maybe Double)] -> (t -> Maybe Int) -> t -> Maybe Double
getDistance distancesToObjects src i = maybe Nothing (\j -> distancesToObjects !! j ! i) $ src i


traverse' ::
  (S.Shape sh', R.Source r a) =>
  Array r sh' a -> (sh' -> a -> b) -> Array D sh' b
traverse' array f = R.traverse array id $ ap f

indexArrayList ::
  (R.Source r1 (Maybe a), R.Source r (Maybe Int), S.Shape sh) =>
  [Array r1 sh (Maybe a)]
  -> Array r sh (Maybe Int) -> Array D sh (Maybe a)
indexArrayList arrayList idxs = traverse' idxs $ \i maybeIdx -> let lookup j  = arrayList !! j ! i
                                                                in  maybe Nothing lookup maybeIdx

indexList ::
  (S.Shape sh, R.Source s (Maybe Int)) =>
  [a] -> Array s sh (Maybe Int) -> Array D sh (Maybe a)
indexList list idxs = traverse' idxs . const . maybe Nothing $ Just . (list !!)


getClosestObjects :: Monad m => m Int -> m Object
getClosestObjects = fmap (objects !!)

checkLight :: Maybe Object -> Bool
checkLight = maybe False _light
                         
filter' :: (U.Unbox a, Monad m, R.Source r a, R.Source s Bool, S.Shape sh1, S.Shape sh2) =>
    Array s sh1 Bool -> Array r sh2 a -> m (Array U DIM1 a)
filterer `filter'` filtered = R.selectP cond constructor sizeFiltered
        where cond              = R.linearIndex filterer . (`quot` width)
              constructor       = R.linearIndex filtered
              sizeFiltered      = S.size $ R.extent filtered
              width             = sizeFiltered `quot` (S.size $ R.extent filterer)



bounce :: (Rays, Array D DIM1 (Array D DIM1 Double)) -> (Rays, Array D DIM1 (Array D DIM1 Double))
bounce (rays, canvas) = fromJust $ do

        -- get values for new rays
        let Rays { _origins   = origins
                 , _vectors   = vectors
                 , _distances = distances
                 , _pixels    = pixels 
                 , _num       = num } = rays

        -- get distances to closest objects
        let distancesToObjects = mapMaybe (distanceFrom rays) objects
              :: [Array D DIM1 (Maybe Double)]
        let closestObjectIdxs  = minIndexArrays distancesToObjects :: Array D DIM1 (Maybe Int)
        let closestDistances   = indexArrayList distancesToObjects closestObjectIdxs
        let closestObjects     = indexList objects closestObjectIdxs :: Array D DIM1 (Maybe Object)

        -- get filter criteria
        let hitLight           = R.map checkLight closestObjects
        let hitNothing         = R.map isNothing closestObjects
        let terminal           = R.zipWith (||) hitLight hitNothing
        let nonterminal        = R.map not terminal :: Array D DIM1 Bool
        let fn = filter' nonterminal :: (U.Unbox a, Monad m, R.Source r a, S.Shape sh2) => (forall a. U.Unbox a => Array r sh2 a) -> m (Array U DIM1 a)
        
        -- filter out terminal
        distances'            <- nonterminal `filter'` R.map (fromMaybe inf) closestDistances
        pixels'               <- nonterminal `filter'` pixels
        origins'              <- nonterminal `filter'` origins
        closestObjectIdxs'    <- nonterminal `filter'` R.map (fromMaybe 0) closestObjectIdxs

        -- get new vectors
        let normals            = getNormals closestObjectIdxs'
        vectors'              <- normals `reflect` vectors
        points                <- march rays distances'

        let rays' = Rays { _origins   = reshape [-1, 3] origins'
                         , _distances = distances'
                         , _pixels    = pixels'
                         , _vectors   = vectors'
                         , _num       = S.size $ R.extent distances' }

        -- Every bounce, multiply color of objects struck by rays with the canvas
        let canvas' = R.zipWith (applyColor canvas) closestObjects pixels

        return (rays', canvas')

applyColor ::
  (R.Source r1 Double, R.Source r (Array r1 (Z :. Int) Double),
   S.Shape (Z :. head)) =>
  Array r (Z :. head) (Array r1 (Z :. Int) Double)
  -> Maybe Object -> head -> Array D (Z :. Int) Double
applyColor canvas object pixelIdx = canvasColor *^ objectColor
  where canvasColor = canvas ! (Z :. pixelIdx)
        objectColor = maybe black _color object
        black       = R.fromFunction (Z :. 3) $ const 0
  
minIndex :: Maybe (Double, Int) -> Int -> [Maybe Double] -> Maybe Int
minIndex Nothing               idx []     = Nothing
minIndex (Just (minX, minIdx)) idx []     = Just minIdx 
minIndex minPair               idx (x:xs) = minIndex minPair' (idx + 1) xs
  where minPair' = case (x, minPair) of
                     (Nothing, _                  ) -> minPair
                     (Just x', Nothing            ) -> Just (x', idx)
                     (Just x', Just (minX, minIdx)) -> if x' < minX
                                                       then Just (x', idx)
                                                       else minPair
    
  

minIndexArrays :: [Array D DIM1 (Maybe Double)] -> Array D DIM1 (Maybe Int)
minIndexArrays [] = error "Cannot take minIndex of empty list"
minIndexArrays arrays@(array:_) = R.fromFunction (R.extent array)
  $ \i -> minIndex Nothing 0 $ map (! i) arrays



origins :: Array D DIM2 Double
origins = R.fromFunction (Z :. 10 :. 3) $ \(Z :. i :. j) -> fromIntegral $ i + j

point :: Array D DIM1 Double
point = R.fromFunction (Z :. 3) $ \(Z :. i) -> fromIntegral i

