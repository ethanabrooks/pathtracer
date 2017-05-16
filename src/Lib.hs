{-# LANGUAGE TemplateHaskell #-}
module Lib ( reshape
           , expandDim
           , flatten
           , mapIndex
           , toSphericalCoords
           , fromSphericalCoords
           , imgHeight
           , imgWidth
           , numIters
           , raysFromCam
           , blankCanvas
           , rZipWith3
           , randomInts
           , rayTrace
           , bounce
           , reflect
           ) where

import qualified Data.Array.Repa     as R -- for Repa
import qualified Data.Array.Repa.Shape as S
import qualified Data.Vector                  as V
import Util (Ray (..), black, white)
import Object (Object (..), Form (..), distanceFrom, objects, march, getNormal)
import Triple (Triple (..), RGB8, Vec3, normalize, tripleToTuple, tripleToList, norm2)
import Data.Array.Repa (Array, DIM1, DIM2, U, D, Z (..), (:.)(..), (!))
import Data.Vector (Vector)
import System.Random
import Data.Maybe
import Data.Angle

-- | Paramters
imgHeight = 100 :: Int --1200
imgWidth  = 100 :: Int --1200
cameraDepth = 50 :: Double

numIters :: Int
numIters = 4
-- |


rZipWith3 :: (R.Source r1 a, R.Source r2 b, R.Source r3 c, S.Shape sh) =>
  (a -> b -> c -> d) ->
  Array r1 sh a ->
  Array r2 sh b ->
  Array r3 sh c ->
  Array D sh d
rZipWith3 = ((R.zipWith id .) .) . R.zipWith


randomInts :: Int -> Int -> Array U DIM1 Int
randomInts len = R.fromListUnboxed (Z :. len) . (take len . randoms) . mkStdGen

raysFromCam :: Array D DIM1 Ray
raysFromCam = flatten $ mapIndex camToPixelRay blankCanvas


camToPixelRay :: DIM2 -> Ray
camToPixelRay (Z :. i :. j) = Ray
  { _origin = pure 0
  , _vector = Triple (fromIntegral i) (fromIntegral j) cameraDepth }


blankCanvas :: Array D DIM2 RGB8
blankCanvas = R.fromFunction (Z :. imgHeight :. imgWidth) $ const white

---

rayTrace :: Int -> Ray -> RGB8 -> RGB8
rayTrace i ray pixel = pixel'
  where (_, _, pixel') = until (isNothing . (\(_, ray, _) -> ray)) update (i, Just ray, pixel)

---

update :: (Int, Maybe Ray, RGB8) -> (Int, Maybe Ray, RGB8)
update (seed, Nothing, pixel)       = (seed, Nothing, pixel)
update (seed, (Just ray), pixel)    =
  case closestTo ray of
    Nothing                  -> (seed, Nothing, black)
    Just (object, distance)  -> stopAtLight object
      where stopAtLight object
              | _light object = (seed, Nothing, _color object)
              | otherwise     = ( getRandom random seed :: Int
                                , bounce seed ray object distance
                                , fmap (`quot` 255) pixel * (_color object))


closestTo :: Ray -> Maybe (Object, Double)
closestTo ray = V.minimumBy closest $ V.map distanceTo objects
  where distanceTo object = fmap ((,) object) (distanceFrom ray $ _form object)


closest :: Maybe (Object, Double) -> Maybe (Object, Double) -> Ordering
closest Nothing _ = GT
closest _ Nothing = LT
closest (Just (_, d1)) (Just (_, d2)) = compare d1 d2

---

bounce :: Int -> Ray -> Object -> Double -> Maybe Ray
bounce i ray object distance = Just $ Ray { _origin = origin, _vector = vector }
  where origin = march ray distance                             :: Vec3
        vector = reflect i object $ _vector ray :: Vec3


reflect :: Int -> Object -> Vec3 -> Triple Double
reflect seed object vector
  | _reflective object = fromSphericalCoords' $ specular seed vAngles nAngles
  | otherwise          = fromSphericalCoords' $ diffuse seed nAngles
  where [vAngles, nAngles] = map toSphericalCoords [-vector, getNormal $ _form object]
        fromSphericalCoords' = uncurry fromSphericalCoords

  --       [thetas, phis] = [map fst angles, map snd angles]
  --       [theta, phi]   = [vectorAngle + 2 * (normalAngle - vectorAngle)
  --                        | [vectorAngle, normalAngle] <- [thetas, phis]]

specular ::
  Int ->
  (Degrees Double , Degrees Double) ->
  (Degrees Double, Degrees Double) ->
  (Degrees Double, Degrees Double)
specular seed (vTheta, vPhi) (nTheta, nPhi) = (theta, phi)
  where noise = getRandom (randomR (0, 0.000001)) seed :: Double
        [theta, phi] = [vAngle + 2 * (nAngle - vAngle) + Degrees noise
                       | (vAngle, nAngle) <- [(vTheta, nTheta), (vPhi, nPhi)]]

diffuse :: Int -> (Degrees Double, Degrees Double) -> (Degrees Double, Degrees Double)
diffuse seed (nTheta, nPhi) = (nTheta + Degrees noise1, nPhi + Degrees noise2)
  where (noise1, gen) = getRandom' $ mkStdGen seed
        (noise2, _)   = getRandom' gen
        getRandom'    = randomR (-90, 90) :: StdGen -> (Double, StdGen)


getRandom :: (StdGen -> (a, b)) -> Int -> a
getRandom randomizer = fst . randomizer . mkStdGen




-- | Util 

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

toSphericalCoords :: Vec3 -> (Degrees Double, Degrees Double)
toSphericalCoords coord = (theta, phi)
  where Triple x y z  = normalize coord
        theta        = arccosine z
        phi | x >  0 = arctangent $ y / x
            | x <  0 = (Degrees 180) + (arctangent $ y / x)
            | x == 0 = Degrees 0
          

fromSphericalCoords :: Degrees Double -> Degrees Double -> Vec3
fromSphericalCoords theta phi = Triple x y z
  where x = cosine phi * sine theta
        y = sine phi * sine theta
        z = cosine theta

-- simpleRGB :: Array D DIM1 RGB8
-- simpleRGB = R.fromFunction (Z :. 2) (\(Z :. i) -> let i' = fromIntegral i :: P.Pixel8
--                                                       in Triple i' (i'+1) (i'+2)) 
-- simpleDim2 :: Array U DIM2 Int
-- simpleDim2 = R.computeS $ R.fromFunction (Z :. 2 :. 3) (\(Z :. i :. j) -> fromIntegral $ i + j)


-- rgb8todim2
--   :: R.Source r RGB8 => Array r DIM1 RGB8 -> Array U DIM2 Int
-- rgb8todim2 array = R.computeS $ R.traverse array (\(Z :. i) -> (Z :. i :. 3))
--   (\src (Z :. i :. j) -> fromIntegral $ tripleToList (src (Z :. i)) !! j)

-- dim2torgb8
--   :: R.Source r Int => Array r DIM2 Int -> Array D DIM1 RGB8
-- dim2torgb8 array = R.traverse array (\(Z :. i :. _) -> (Z :. i))
--   (\src (Z :. i) -> let get j = fromIntegral $ src (Z :. i :. j)
--                         in Triple (get 0) (get 1) (get 2))
