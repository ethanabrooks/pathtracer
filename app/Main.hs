{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE FlexibleContexts #-}

module Main ( main
            , RGB8
            ) where

import qualified Codec.Picture.Types          as M
import qualified Data.Array.Repa              as R 
import qualified Data.Array.Repa.Shape        as S
import qualified Data.Array.Repa.Repr.Unboxed as U
import qualified Data.Vector                  as V
import qualified Codec.Picture                as P

import Data.Array.Repa (Array, DIM1, DIM2, U, D, Z (..), (:.)(..), (!))
import System.Environment (getArgs)
import System.FilePath (replaceExtension)
import Object (Object (..), Form (..), distanceFrom, objects, march, getNormal)
import Triple (Triple (..), RGB8, Vec3, normalize, tripleToTuple, tripleToList)
import Lib (flatten, reshape, Ray (..), mapIndex, black,
            white, toSphericalCoords, fromSphericalCoords)
import Data.Vector (Vector)
import Data.Maybe
import Control.Monad
import Control.Monad.Loops
import Debug.Trace
import System.Random


toImage :: (R.Source r RGB8, S.Shape sh) => Array r sh RGB8 -> P.DynamicImage
toImage canvas = P.ImageRGB8 $ P.generateImage fromCoords imgHeight imgWidth
  where fromCoords i j = convert $ canvas' ! (Z :. i :. j)
        convert (Triple r g b) = P.PixelRGB8 r g b
        canvas' = reshape [imgHeight, imgWidth] canvas

-- | Paramters
imgHeight = 100 :: Int --1200
imgWidth  = 100 :: Int --1200
cameraDepth = 50 :: Double

numIters :: Int
numIters = 4
-- |

main :: IO ()
main = do (_, canvas) <- mainLoop
          (P.savePngImage "image.png" . toImage) canvas

rZipWith3 :: (R.Source r1 a, R.Source r2 b, R.Source r3 c, S.Shape sh) =>
  (a -> b -> c -> d) ->
  Array r1 sh a ->
  Array r2 sh b ->
  Array r3 sh c ->
  Array D sh d
rZipWith3 = ((R.zipWith id .) .) . R.zipWith

mainLoop :: IO (Int, Array D DIM1 RGB8)
mainLoop = iterateUntilM ((== numIters) . fst)
  (\(n, canvas) -> do let randomSeeds = randomInts (imgHeight * imgWidth) n
                      return (n + 1, rZipWith3 rayTrace randomSeeds raysFromCam canvas))
  (1, flatten blankCanvas)

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
              | _light object = (seed, Nothing, white)
              | otherwise     = ( getRandom random seed :: Int
                                , bounce seed ray object distance
                                , pixel')
                  where pixel' = fmap ((`quot` 255)) pixel * (_color object)
                -- in trace ("pixel: " ++ show pixel ++
                --                        "\ncolor: " ++ show (_color object))
                -- ( getRandom random seed :: Int
                --                 , bounce seed ray object distance
                --                 , trace ("product: " ++ show pixel') pixel')


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

specular
  :: Int -> (Double, Double) -> (Double, Double) -> (Double, Double)
specular seed (vTheta, vPhi) (nTheta, nPhi) = (theta, phi)
  where noise = getRandom (randomR (0, 0.000001)) seed :: Double
        [theta, phi] = [vAngle + 2 * (nAngle - vAngle) + noise
                       | (vAngle, nAngle) <- [(vTheta, nTheta), (vPhi, nPhi)]]

diffuse :: Int -> (Double, Double) -> (Double, Double)
diffuse seed (nTheta, nPhi) = (nTheta + noise1, nPhi + noise2)
  where (noise1, gen) = getRandom' $ mkStdGen seed
        (noise2, _) = getRandom' gen
        getRandom' = randomR (-pi / 2, pi / 2) :: StdGen -> (Double, StdGen)

rand :: IO Double
rand = getStdRandom (randomR (-pi / 2, pi / 2))
  

getRandom :: (StdGen -> (a, b)) -> Int -> a
getRandom randomizer = fst . randomizer . mkStdGen



