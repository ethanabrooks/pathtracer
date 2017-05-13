{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Object2 where

import qualified Data.Array.Repa     as R -- for Repa
import qualified Data.Array.Repa.Shape as S
import qualified Data.Array.Repa.Algorithms.Matrix as M
import qualified Data.Array.Repa.Repr.Unboxed as U
import qualified Codec.Picture as P

import Data.Array.Repa (Array, DIM1, DIM2, DIM3, U, D, Z (..), (:.)(..),
                        (!), (++), (*^), (+^), (-^), (/^))
import Data.Array.Repa.Slice (Any (..), All (..))
-- import Lib (Vec3, Vec1, Rays (..), _num, _origins, _distances, inf,
--             lessThan, filterWith, emptyRays, reshape, expandDim)
import Lib (reshape, expandDim, lessThan, flatten)
import qualified Data.Vector as V
import Data.Vector (Vector)
import Data.Maybe
import Control.Monad
import Prelude hiding ((++))
import Control.Exception.Base
import Control.Applicative

type RGB8 = Triple P.Pixel8

data Triple a = Triple a a a
type Vec3 = Triple Double
type Canvas = Array D DIM2 RGB8
  
data Form = Disk
            { _center :: Vec3
            , _normal :: Vec3
            , _radius :: Double }
          | InfinitePlane
            { _normal :: Vec3
            , _point  :: Vec3 }
 
data Object = Object { _color      :: RGB8
                     , _light      :: Bool
                     , _reflective :: Bool
                     , _form       :: Form }

data Ray = Ray { _origin   :: Vec3
               , _vector   :: Vec3 }

-- | Paramters
imgHeight = 2 :: Int --1200
imgWidth  = 4 :: Int --1200
cameraDepth = 10 :: Double

black = pure 0 :: RGB8
white = pure 1 :: RGB8
some_vec = pure 1

infPlane = Object 
  { _color      = black
  , _light      = False
  , _reflective = True
  , _form       = InfinitePlane
             { _normal = some_vec
             , _point  = some_vec }
  }


objects :: Vector Object
objects = V.fromList [infPlane] 

canvas :: Canvas
canvas = R.fromFunction (Z :. imgHeight :. imgWidth) $ const black

blankCanvas :: Canvas
blankCanvas = R.fromFunction (Z :. imgHeight :. imgWidth) $ const white


mapWithIndex ::
  (S.Shape sh', R.Source r a) =>
  (sh' -> a -> b) -> Array r sh' a -> Array D sh' b
mapWithIndex f array = R.traverse array id $ ap f

mapIndex ::
  (S.Shape sh', R.Source r a) =>
  (sh' -> b) -> Array r sh' a -> Array D sh' b
mapIndex f array = R.traverse array id $ const f

tsum :: Num a => Triple a -> a
tsum = sum . tripleToList

norm2 :: Floating a => Triple a -> a
norm2 (Triple x y z) = sqrt $ x^2 + y^2 + z^2

normalize :: Vec3 -> Vec3
normalize vector = fmap (/ norm) vector
  where norm = max (10^(-6)) $ norm2 vector

raysFromCam :: Array D DIM1 Ray
raysFromCam = flatten $ mapIndex camToPixelRay canvas

camToPixelRay :: DIM2 -> Ray
camToPixelRay (Z :. i :. j)  = Ray { _origin   = pure 0
                                   , _vector   = Triple i' j' cameraDepth }
  where (i', j') = (fromIntegral i, fromIntegral j)


tripleToTuple :: Triple t -> (t, t, t)
tripleToTuple (Triple a1 a2 a3) = (a1, a2, a3)

tripleToList :: Triple t -> [t]
tripleToList (Triple a1 a2 a3) = [a1, a2, a3]

instance Show a => Show (Triple a) where
  show = show . tripleToTuple
  
instance Functor (Triple) where
  fmap f (Triple a1 a2 a3) = Triple (f a1) (f a2) (f a3)

instance Applicative (Triple) where
  pure a = Triple a a a
  Triple f1 f2 f3 <*> Triple a1 a2 a3 = Triple (f1 a1) (f2 a2) (f3 a3)

instance Num a => Num (Triple a) where 
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  abs = fmap abs
  signum = fmap signum
  fromInteger = fmap fromInteger . pure
  negate = fmap negate

march :: Ray -> Double -> Vec3
march Ray{ _origin = origin, _vector = vector } distance = origin + fmap (distance *) vector

inf :: Double
inf = 1 / 0

dot a b = tsum $ a * b 


distanceFrom' ray@Ray { _origin = origin, _vector = vector } form =
  case form of
    Disk { _center = center, _normal = normal, _radius = radius }  -> do
      distanceFromOrigin    <- distanceFrom ray
                               $ InfinitePlane { _point = center , _normal = normal }
      let point              = march ray distanceFromOrigin
      let distanceFromCenter = norm2 $ point - center
      guard $ distanceFromCenter < radius
      return distanceFromOrigin
    InfinitePlane { _point = point, _normal = normal } ->
        Just $ (point - origin) `dot` normal / vector `dot` normal


distanceFrom :: Ray -> Form -> Maybe Double
distanceFrom ray@Ray { _origin = origin, _vector = vector } form =
  do distance <- distanceFrom' ray form
     guard $ distance > 0 
     return distance
  
  
getNormal :: Form -> Vec3
getNormal form = _normal form

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


reflect :: Vec3 -> Vec3 -> Triple Double
normal `reflect` vector = fromSphericalCoords theta phi
  where angles         = map toSphericalCoords [-vector, normal]
        [thetas, phis] = [map fst angles, map snd angles]
        [theta, phi]   = [vectorAngle + 2 * (normalAngle - vectorAngle)
                         | [vectorAngle, normalAngle] <- [thetas, phis]]

closest :: Maybe Double -> Maybe Double -> Ordering
closest d1 d2 = compare d1' d2'
  where [d1', d2'] = map (fromMaybe inf) [d1, d2]


getClosestObject :: Ray -> Maybe Object
getClosestObject ray = fmap (const closestObject) closestDistance
  where distances          = V.map (distanceFrom ray . _form) objects   :: Vector (Maybe Double)
        compareSnd (_, d1) (_, d2) = closest d1 d2
        (closestObject, closestDistance) = V.minimumBy compareSnd $ V.zip objects distances
  
addColor :: Maybe Object -> RGB8 -> RGB8
addColor object pixel = maybe black ((pixel *) . _color) object


bounce ray object distance = Just $ Ray { _origin = origin, _vector = vector }
  where origin = march ray distance                             :: Vec3
        vector = getNormal (_form object) `reflect` _vector ray :: Vec3


closestTo :: Ray -> (Maybe Double, Object)
closestTo ray = V.minimumBy (\(d1, _) (d2, _) -> closest d1 d2) $ V.zip distances objects
                                                             :: (Maybe Double, Object) 
  where distances = V.map (distanceFrom ray . _form) objects :: Vector (Maybe Double)


allTerminal ::
  (R.Source r (Maybe a), Monad m, S.Shape sh) =>
  Array r sh (Maybe a) -> m Bool
allTerminal rays = R.foldAllP (&&) True isTerminal
  where isTerminal = R.map isNothing rays


update :: RGB8 -> Ray -> (RGB8, Maybe Ray)
update pixel ray = (addColor object' pixel, bounce ray object =<< mDistance)
  where (mDistance, object) = closestTo ray
        object' = fmap (const object) mDistance

rayTrace :: Array D DIM1 (RGB8, Maybe Ray)
rayTrace = until allRaysTerminal updateAll $ R.zipWith (,) canvas' rays'
                                           
  where allRaysTerminal :: Array D DIM1 (RGB8, Maybe Ray) -> Bool
        allRaysTerminal = fromJust . allTerminal . R.map snd

        updateIfNonTerminal :: RGB8 -> Maybe Ray -> (RGB8, Maybe Ray)
        updateIfNonTerminal = maybe (black, Nothing) . update

        updateAll :: Array D DIM1 (RGB8, Maybe Ray) -> Array D DIM1 (RGB8, Maybe Ray)
        updateAll = R.map . uncurry $ updateIfNonTerminal

        canvas' = flatten blankCanvas    :: Array D DIM1 RGB8
        rays'   = R.map Just raysFromCam :: Array D DIM1 (Maybe Ray)



 {-
TODO:
refactor into separate files
write tests
add diffuse reflectivity
add spheres
-}
        
