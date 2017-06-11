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

import qualified Data.Array.Repa       as R -- for Repa
import qualified Data.Array.Repa.Shape as S
import qualified Data.Vector           as V
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
cameraDepth = 100 :: Double

numIters :: Int
numIters = 1000
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
  { _origin = Triple 0 0 (-cameraDepth)
  , _vector = normalize $ Triple i' j' cameraDepth }
  where i' = fromIntegral i - fromIntegral imgHeight / 2
        j' = fromIntegral j - fromIntegral imgWidth / 2


blankCanvas :: Array D DIM2 Vec3
blankCanvas = R.fromFunction (Z :. imgHeight :. imgWidth) $ const black

---

rayTrace :: Int -> StdGen -> Ray -> Vec3 -> Vec3
rayTrace n gen ray pixel = pixel + pixel'
  where (_, _, pixel') = until (isNothing . (\(_, ray, _) -> ray)) update (gen, Just ray, white)
        x = fromIntegral n

---

update :: (StdGen, Maybe Ray, Vec3) -> (StdGen, Maybe Ray, Vec3)
update (gen, Nothing, pixel)  = (gen, Nothing, pixel) 
update (gen, Just ray, pixel) =
  case closestTo ray of
    Nothing                  -> (gen, Nothing, black)
    Just (object, distance)  -> stopAtLight
      where stopAtLight | _emittance object > 0 = ( gen, Nothing
                                                  , fmap (_emittance object *) pixel )
                        | otherwise             = ( snd (random gen :: (Int, StdGen))
                                                  , Just $ bounce gen ray object distance
                                                  , fmap (/ 255) $ pixel * _color object )
      -- ("\n" ++ _name object
      --                                  ++ ":\npixel-color=" ++ (show pixel)
      --                                  ++ "\nemmitance=" ++ (show $ _emittance object)
      --                                  ++ "\nobject-color=" ++ (show $ _color object)
      --                                  ++ "\ncombination=" ++ (show $ (\(_, _, c) -> c) stopAtLight))

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
        -- (phi, _)      = randomAngle gen' (0, 380)
  -- where [theta, phi] = map (randomAngle gen) [(0, 90), (0, 360)]
  -- TODO!!!
  -- where [theta, phi] = map (randomAngle gen) [(90, 180), (0, 360)]
