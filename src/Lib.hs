{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}
module Lib ( imgHeight
           , imgWidth
           , numIters
           , raysFromCam
           , blackCanvas
           , mainLoop
           , mainLoop'
           , rZipWith3
           , randomGens
           , rayTrace
           , bounce
           , reflect
           , specular
           ) where

import qualified Data.Array.Repa       as R -- for Repa
import qualified Data.Array.Repa.Shape as S
import qualified Data.Vector           as V
import Util
import Object
import Triple
import Data.Array.Repa (Array, DIM1, DIM2, U, D, Z (..), (:.)(..), (!), (+^))
import Data.Vector (Vector)
import System.Random
import Data.Maybe
import Data.Angle
import Debug.Trace


-- | Parameters
imgHeight = 30 :: Int --1200
imgWidth  = 30 :: Int --1200
cameraDepth = 100 :: Double
numIters = 10 :: Int
-- |


rZipWith3 :: (R.Source r1 a, R.Source r2 b, R.Source r3 c, S.Shape sh) =>
  (a -> b -> c -> d) ->
  Array r1 sh a ->
  Array r2 sh b ->
  Array r3 sh c ->
  Array D sh d
rZipWith3 = ((R.zipWith id .) .) . R.zipWith


randomGens :: Int -> Int -> Array D DIM1 StdGen
randomGens len = R.map mkStdGen . R.fromListUnboxed (Z :. len) . (take len . randoms) . mkStdGen


raysFromCam :: Array D DIM1 Ray
raysFromCam = flatten $ mapIndex camToPixelRay blackCanvas


camToPixelRay :: DIM2 -> Ray
camToPixelRay (Z :. i :. j) = Ray
  { _origin = Triple 0 0 (-cameraDepth)
  , _vector = normalize $ Triple i' j' cameraDepth }
  where i' = fromIntegral i - fromIntegral imgHeight / 2
        j' = fromIntegral j - fromIntegral imgWidth / 2


blackCanvas :: Array D DIM2 Vec3
blackCanvas = R.fromFunction (Z :. imgHeight :. imgWidth) $ const black

whiteCanvas :: Array D DIM1 Vec3
whiteCanvas = R.fromFunction (Z :. imgHeight * imgWidth) $ const black


mainLoop :: Int -> Array D DIM1 Vec3 -> (Int, Array D DIM1 Vec3)
mainLoop n canvas = (n + 1, canvas +^ rZipWith3 rayTrace gens raysFromCam whiteCanvas)
    where gens = randomGens (imgHeight * imgWidth) n

mainLoop' :: Int -> Array D DIM1 Vec3 -> Array D DIM1 Vec3
mainLoop' 0 canvas = canvas
mainLoop' n canvas = mainLoop' (n - 1) (canvas +^ rZipWith3 rayTrace gens raysFromCam whiteCanvas)
    where gens = randomGens (imgHeight * imgWidth) n


---

rayTrace' :: StdGen -> Ray -> Vec3 -> Vec3
rayTrace' gen ray pixel = pixel'
  where (_, _, pixel') = until (isNothing . (\(_, ray, _) -> ray)) update (gen, Just ray, white)

rayTrace :: StdGen -> Ray -> Vec3 -> Vec3
rayTrace gen ray pixel =
  case update (gen, Just ray, pixel) of
    (_, Nothing, pixel') -> pixel'
    (gen', Just ray', pixel') -> rayTrace' gen' ray' pixel'

---

update :: (StdGen, Maybe Ray, Vec3) -> (StdGen, Maybe Ray, Vec3)
update (gen, Nothing, !pixel)  = (gen, Nothing, pixel) 
update (gen, Just !ray, !pixel) =
  case closestTo ray of
    Nothing                  -> (gen, Nothing, black)
    Just (object, distance)  -> stopAtLight
      where stopAtLight | _emittance object > 0 = ( gen, Nothing
                                                  , fmap (_emittance object *) pixel )
                        | otherwise             = ( snd (random gen :: (Int, StdGen))
                                                  , Just $ bounce gen ray object distance
                                                  , fmap (/ 255) $ pixel * _color object )

closestTo :: Ray -> Maybe (Object, Double)
closestTo ray = V.minimumBy closest $ V.map distanceTo objects
  where distanceTo object = fmap (object,) (distanceFrom ray $ _form object)


closest :: Maybe (Object, Double) -> Maybe (Object, Double) -> Ordering
closest Nothing _ = GT
closest _ Nothing = LT
closest (Just (_, d1)) (Just (_, d2)) = compare d1 d2

---

bounce :: StdGen -> Ray -> Object -> Double -> Ray
bounce gen ray object distance = Ray origin vector
  where origin = march ray distance               :: Vec3
        vector = reflect gen object $ _vector ray :: Vec3


reflect :: StdGen -> Object -> Vec3 -> Vec3
reflect gen object vector
  | _reflective object = specular gen 0 vector normal
  | otherwise          = diffuse gen vector normal
  where normal = getNormal $ _form object


specular :: StdGen -> Double -> Vec3 -> Vec3 -> Vec3
specular gen noise vector normal = rotateRel theta phi vector'
  where normal'          = normalize normal
        projection       = fmap (vector `dot` normal' *) normal'
        vector'          = vector + (fmap ((-2) *) projection)

        -- here we offset the angle of reflection by `noise` but ensure that this does not
        -- cause rays to penetrate the surface of the object
        angleWithSurface = (Degrees 90) - (arccosine . abs $ normal' `dot` normalize vector)
        Degrees maxTheta = min angleWithSurface $ Degrees noise 
        [theta, phi] = map Degrees . fst $ randomRangeList gen [(0, maxTheta), (0, 380)]
        

diffuse :: StdGen -> Vec3 -> Vec3 -> Vec3 
diffuse gen _ normal = rotateRel theta phi normal
  where [theta, phi] = map Degrees . fst $ randomRangeList gen [(0, 90), (0, 380)]
  -- where (theta', gen') = randomR (0, 90) gen
  --       (phi', _) = randomR (0, 380) gen
  --       [theta, phi] = map Degrees [theta', phi']
