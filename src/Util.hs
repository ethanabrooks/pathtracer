{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE Strict #-}

module Util where

import qualified Codec.Picture as P
import Control.Applicative
import Control.Exception.Base (assert)
import Data.Angle
       (Degrees(..), arctangent, arccosine, cosine, sine)
import Data.Array.Accelerate (Lift(..), Plain)
import Data.Array.Accelerate.Array.Sugar
       (Elt(..), EltRepr, Tuple(..), TupleRepr)
import Data.Array.Accelerate.Product (IsProduct(..))
import Data.Array.Accelerate.Smart
import Data.Array.Repa ((!))
import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.Shape as S
import qualified Data.ByteString.Base64
import qualified Data.ByteString.Lazy.Char8
import Data.Fixed (mod')
import Data.Foldable
import Data.Maybe
import Data.Range.Range (Range)
import qualified Data.Text.Encoding
import qualified Data.Text.Lazy as TL
import Data.Typeable (Typeable)
import qualified Params
import qualified System.Random as Random
import Text.Read (readMaybe)
import Triple (Vec3, Triple(..), normalize, norm2)

type instance EltRepr Random.StdGen = EltRepr (Int, Int)

readErrorMsg :: String -> String -> String
readErrorMsg string typeString =
  "Failed to read string: \"" ++ string ++ "\" as " ++ typeString ++ "."

stdGenToTuple :: Random.StdGen -> Maybe (Int, Int)
stdGenToTuple gen =
  case map readMaybe strings :: [Maybe Int] of
    [Just a, Just b] -> Just (a, b)
    _ -> Nothing
  where
    strings = words $ show gen :: [String]

unsafeStdGenToTuple :: Random.StdGen -> (Int, Int)
unsafeStdGenToTuple gen =
  fromMaybe (error $ readErrorMsg (show gen) "[Int]") $ stdGenToTuple gen

instance Elt Random.StdGen where
  eltType _ = eltType (undefined :: (Int, Int))
  toElt :: EltRepr (Int, Int) -> Random.StdGen
  toElt p =
    fromMaybe (error $ readErrorMsg string "Random.StdGen") (readMaybe string) :: Random.StdGen
    where
      (a, b) = toElt p :: (Int, Int)
      string = show a ++ " " ++ show b
  fromElt :: Random.StdGen -> EltRepr (Int, Int)
  fromElt = fromElt . unsafeStdGenToTuple

instance IsProduct Elt Random.StdGen where
  type ProdRepr Random.StdGen = ProdRepr (Int, Int)
  fromProd cst = fromProd cst . unsafeStdGenToTuple
  toProd cst = toElt
  prod cst _ = prod cst (undefined :: (Int, Int))

instance Lift Exp Random.StdGen where
  type Plain Random.StdGen = (Int, Int)
  lift gen = Exp . Tuple $ NilTup `SnocTup` lift a `SnocTup` lift b
    where
      (a, b) = unsafeStdGenToTuple gen

newtype Point =
  Point (Triple Double)
  deriving (Eq, Show)

newtype Vector =
  Vector Vec3
  deriving (Eq, Show)

newPoint a b c = Point $ Triple a b c

newVector a b c = Vector $ Triple a b c

fromTripleArray
  :: R.Source r (Triple a)
  => R.Array r R.DIM2 (Triple a) -> R.Array R.D R.DIM3 a
fromTripleArray array =
  R.fromFunction
    (R.Z R.:. rows R.:. cols R.:. 3)
    (\(R.Z R.:. i R.:. j R.:. k) ->
       let Triple x y z = (array ! (R.Z R.:. i R.:. j))
       in [x, y, z] !! k)
  where
    (R.Z R.:. rows R.:. cols) = R.extent array

toTripleArray
  :: R.Source r a
  => R.Array r R.DIM3 a -> R.Array R.D R.DIM2 (Triple a)
toTripleArray array =
  R.fromFunction
    (R.Z R.:. rows R.:. cols)
    (\(R.Z R.:. i R.:. j) ->
       let [x, y, z] = [array ! (R.Z R.:. i R.:. j R.:. k) | k <- [0 .. 2]]
       in Triple x y z)
  where
    (R.Z R.:. rows R.:. cols R.:. _) = R.extent array

mapIndex
  :: (S.Shape sh', R.Source r a)
  => (sh' -> b) -> R.Array r sh' a -> R.Array R.D sh' b
mapIndex f array = R.traverse array id $ const f

inferMissing
  :: (Show a, Integral a)
  => [a] -> [a] -> [a]
inferMissing list listWithNeg =
  (assert =<< valid)
    [ if x < 0
      then missingValue
      else x
    | x <- listWithNeg
    ]
  where
    valid result = (product result == product list) && all (> 0) result
    missingValue = product list `quot` product (filter (>= 0) listWithNeg)

reshape
  :: (R.Source r1 e, S.Shape sh1, S.Shape sh2)
  => [Int] -> R.Array r1 sh1 e -> R.Array R.D sh2 e
reshape shape array = R.reshape (S.shapeOfList shape') array
  where
    shape' = inferMissing (S.listOfShape (R.extent array)) shape

expandDim
  :: (R.Source r1 e, S.Shape sh1, S.Shape sh2)
  => Int -> R.Array r1 sh1 e -> R.Array R.D sh2 e
expandDim dim array = R.reshape shape array
  where
    shape = S.shapeOfList . insertAt dim 1 . S.listOfShape $ R.extent array

insertAt :: Int -> a -> [a] -> [a]
insertAt n x list = take n list ++ [x] ++ drop n list

instance Functor Degrees where
  fmap f (Degrees x) = Degrees (f x)

arctan2 :: Double -> Double -> Degrees Double
arctan2 x y = (`mod'` 360.0) <$> arctan'
  where
    arctan = arctangent $ y / x
    arctan'
      | x < 0 = arctan + Degrees 180
      | x >= 0 = arctan

toSphericalCoords :: Vec3 -> (Degrees Double, Degrees Double)
toSphericalCoords vec
  | all (0 ==) [x, y] && z >= 0 = (0, 0)
  | all (0 ==) [x, y] && z < 0 = (Degrees 180, 0)
  | otherwise = (theta, phi)
  where
    Triple x y z = normalize vec
    theta = arccosine z
    phi = arctan2 x y

fromSphericalCoords :: Degrees Double -> Degrees Double -> Vec3
fromSphericalCoords theta phi = Triple x y z
  where
    x = cosine phi * sine theta
    y = sine phi * sine theta
    z = cosine theta

rotateAbs :: Vec3 -> Degrees Double -> Degrees Double -> Vec3
rotateAbs (Triple x y z) theta phi = Triple x' y' z'
  where
    x' =
      cosine theta * cosine phi * x - sine phi * y + sine theta * cosine phi * z
    y' =
      sine phi * cosine theta * x + cosine phi * y + sine phi * sine theta * z
    z' = -sine theta * x + cosine theta * z

rotateRel :: Degrees Double -> Degrees Double -> Vec3 -> Vec3
rotateRel theta phi vector = (* length) <$> rotateAbs vector' theta' phi'
  where
    vector' = fromSphericalCoords theta phi
    (theta', phi') = toSphericalCoords vector
    length = norm2 vector

aeq
  :: (Num a, Ord a)
  => a -> a -> a -> Bool
aeq tolerance a b = (a - b) * (a - b) < tolerance

vecAeq
  :: (Ord a, Num a)
  => a -> Triple a -> Triple a -> Bool
vecAeq tolerance a b = and $ liftA2 (aeq tolerance) a b

contains
  :: Ord t
  => (t, Bool, t, Bool) -> t -> Bool
contains (low, lowInclusive, high, highInclusive) x = aboveLow && belowHigh
  where
    aboveLow
      | lowInclusive = low <= x
      | otherwise = low < x
    belowHigh
      | highInclusive = x <= high
      | otherwise = x < high

randomAngle
  :: (Random.RandomGen b, Random.Random x, Show x)
  => b -> (x, x) -> (Degrees x, b)
randomAngle gen range = (Degrees angle, gen')
  where
    (angle, gen') = Random.randomR range gen

randomRangeList
  :: (Random.RandomGen b, Random.Random a, Show a)
  => b -> [(a, a)] -> ([a], b)
randomRangeList firstGen ranges = (reverse randoms, lastGen)
  where
    (randoms, lastGen) =
      foldl
        (\(xs, gen) range ->
           let (x, gen') = Random.randomR range gen
           in (x : xs, gen'))
        ([], firstGen)
        ranges

listToPixelRGB8 :: [Double] -> P.PixelRGB8
listToPixelRGB8 list = P.PixelRGB8 r g b
  where
    [r, g, b] = round . (255 *) <$> list

repa3ToImage
  :: (R.Source r Double)
  => R.Array r R.DIM3 Double -> P.Image P.PixelRGB8
repa3ToImage canvas = P.generateImage fromCoords Params.height Params.width
  where
    fromCoords i j =
      listToPixelRGB8 [canvas ! (R.Z R.:. i R.:. j R.:. k) | k <- [0 .. 2]]

repa2ToImage
  :: (R.Source r Vec3)
  => R.Array r R.DIM2 Vec3 -> P.Image P.PixelRGB8
repa2ToImage canvas = P.generateImage fromCoords Params.height Params.width
  where
    fromCoords i j =
      let Triple x y z = canvas ! (R.Z R.:. i R.:. j)
      in listToPixelRGB8 [x, y, z]

imageToText :: P.Image P.PixelRGB8 -> TL.Text
imageToText =
  TL.fromStrict .
  Data.Text.Encoding.decodeUtf8 .
  Data.ByteString.Base64.encode .
  Data.ByteString.Lazy.Char8.toStrict . P.encodePng

repa2ToText
  :: (R.Source r Vec3)
  => R.Array r R.DIM2 Vec3 -> TL.Text
repa2ToText = imageToText . repa2ToImage

repa1ToText
  :: (R.Source r Vec3)
  => R.Array r R.DIM1 Vec3 -> TL.Text
repa1ToText = imageToText . repa2ToImage . reshape [Params.height, Params.width]

repa3ToText :: R.Array R.U R.DIM3 Double -> TL.Text
repa3ToText = imageToText . repa3ToImage

clamp
  :: Ord a
  => a -> a -> a -> a
clamp lower upper = max lower . min upper
