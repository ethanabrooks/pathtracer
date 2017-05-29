{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
module Lib ( imgHeight
           , imgWidth
           , numIters
           , raysFromCam
           , blankCanvas
           , rZipWith3
           , randomGens
           , rayTrace
           , bounce
           , reflect
           , specular
           ) where

import qualified Data.Array.Repa     as R -- for Repa
import qualified Data.Array.Repa.Shape as S
import qualified Data.Vector                  as V
import Util
import Object
import Triple
import Data.Array.Repa (Array, DIM1, DIM2, U, D, Z (..), (:.)(..), (!))
import Data.Vector (Vector)
import System.Random
import Data.Maybe
import Data.Angle
import Debug.Trace

-- | Paramters
imgHeight = 100 :: Int --1200
imgWidth  = 100 :: Int --1200
cameraDepth = 10 :: Double

numIters :: Int
numIters = 2
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
raysFromCam = flatten $ mapIndex camToPixelRay blankCanvas


camToPixelRay :: DIM2 -> Ray
camToPixelRay (Z :. i :. j) = Ray
  { _origin = pure 0
  , _vector = Triple (fromIntegral i) (fromIntegral j) cameraDepth }


blankCanvas :: Array D DIM2 RGB8
blankCanvas = R.fromFunction (Z :. imgHeight :. imgWidth) $ const white

---

rayTrace :: StdGen -> Ray -> RGB8 -> RGB8
rayTrace gen ray pixel = pixel'
  where (_, _, pixel') = until (isNothing . (\(_, ray, _) -> ray)) update (gen, Just ray, pixel)

---

update :: (StdGen, Maybe Ray, RGB8) -> (StdGen, Maybe Ray, RGB8)
update (gen, Nothing, pixel)  = (gen, Nothing, pixel)
update (gen, Just ray, pixel) =
  case closestTo ray of
    -- Nothing                  -> (gen, Nothing, black)
    Nothing                  -> trace "Nothing" (gen, Nothing, black)
    Just (object, distance)  -> stopAtLight
      -- where stopAtLight | _light object = ( gen, Nothing, pixel * _color object )
      where stopAtLight | _light object = trace "Light" ( gen, Nothing, traceShowId $ pixel * (traceShowId $ _color object) )
                        -- | otherwise     = ( snd (random gen :: (Int, StdGen))
                        | otherwise     = trace "?" ( snd (random gen :: (Int, StdGen))
                                          , Just $ bounce gen ray object distance
                                          , pixel * _color object )


closestTo :: Ray -> Maybe (Object, Double)
closestTo ray = V.minimumBy closest $ V.map distanceTo objects
  where distanceTo object = let x = fmap (object,) (distanceFrom ray $ _form object)
                                in trace (show $ maybe "No object" (_name . fst) x) x


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
specular gen noise vector normal = vector'
  where normal'          = normalize normal
        projection       = fmap (vector `dot` normal' *) normal'
        vector'          = vector + (fmap ((-2) *) projection)

        -- here we offset the angle of reflection by `noise` but ensure that this does not
        -- cause rays to penetrate the surface of the object
        angleWithSurface = (Degrees 90) - (arccosine . abs $ normal' `dot` normalize vector)
        Degrees maxTheta = min angleWithSurface $ Degrees noise 
        [theta, phi]     = map (randomAngle gen) [(0, maxTheta), (0, 360)]
        

diffuse :: StdGen -> Vec3 -> Vec3 -> Vec3 
diffuse gen vector normal = rotateRel theta phi vector
  where [theta, phi] = map (randomAngle gen) [(0, 90), (0, 360)]
