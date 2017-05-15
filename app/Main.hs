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
import Control.Monad.Random.Class


toImage :: (R.Source r RGB8, S.Shape sh) => Array r sh RGB8 -> P.DynamicImage
toImage canvas = P.ImageRGB8 $ P.generateImage fromCoords imgHeight imgWidth
  where fromCoords i j = convert $ canvas' ! (Z :. i :. j) * 255
        convert (Triple r g b) = P.PixelRGB8 r g b
        canvas' = reshape [imgHeight, imgWidth] canvas

-- | Paramters
imgHeight = 150 :: Int --1200
imgWidth  = 150 :: Int --1200
cameraDepth = 10 :: Double

numIters :: Int
numIters = 2
-- |

main :: IO ()
main = do (_, canvas) <- mainLoop
          (P.savePngImage "image.png" . toImage) canvas


mainLoop :: IO (Int, Array D DIM1 RGB8)
mainLoop = iterateUntilM ((== numIters) . fst)
  (\(n, canvas) -> do f r c =  
                      canvas' <- R.zipWith (fmap rayTrace) raysFromCam canvas
                      return (n + 1, canvas'))
  (1, flatten blankCanvas)


raysFromCam :: Array D DIM1 Ray
raysFromCam = flatten $ mapIndex camToPixelRay blankCanvas


camToPixelRay :: DIM2 -> Ray
camToPixelRay (Z :. i :. j) = Ray
  { _origin = pure 0
  , _vector = Triple (fromIntegral i) (fromIntegral j) cameraDepth }


blankCanvas :: Array D DIM2 RGB8
blankCanvas = R.fromFunction (Z :. imgHeight :. imgWidth) $ const white

---

rayTrace :: Ray -> RGB8 -> IO RGB8
rayTrace ray pixel = do (Just r, p) <- iterateUntilM (isNothing . fst) update (Just ray, pixel)
                        return p

---

update :: (Maybe Ray, RGB8) -> IO (Maybe Ray, RGB8)
update (Nothing, pixel)       = return (Nothing, pixel)
update ((Just ray), pixel)    =
  case closestTo ray of
    Nothing                  -> return (Nothing, black)
    Just (object, distance)  -> stopAtLight object
      where stopAtLight object
              | _light object = return (Nothing, pixel)
              | otherwise     = do ray' <- bounce ray object distance
                                   return (ray', pixel * _color object)


closestTo :: Ray -> Maybe (Object, Double)
closestTo ray = V.minimumBy closest $ V.map distanceTo objects
  where distanceTo object = fmap ((,) object) (distanceFrom ray $ _form object)


closest :: Maybe (Object, Double) -> Maybe (Object, Double) -> Ordering
closest Nothing _ = GT
closest _ Nothing = LT
closest (Just (_, d1)) (Just (_, d2)) = compare d1 d2

---

bounce :: Ray -> Object -> Double -> IO (Maybe Ray)
bounce ray object distance = do 
  let origin = march ray distance
  vector <- object `reflect` _vector ray 
  return . Just $ Ray { _origin = origin, _vector = vector }


reflect :: Object -> Vec3 -> IO Vec3
object `reflect` vector
  | _reflective object = do (theta, phi) <- specular vAngles nAngles
                            return $ fromSphericalCoords theta phi
  | otherwise          = return vector -- uncurry fromSphericalCoords $ diffuse nAngles
  where [vAngles, nAngles] = map toSphericalCoords [-vector, getNormal $ _form object]
  --       [thetas, phis] = [map fst angles, map snd angles]
  --       [theta, phi]   = [vectorAngle + 2 * (normalAngle - vectorAngle)
  --                        | [vectorAngle, normalAngle] <- [thetas, phis]]

specular (vTheta, vPhi) (nTheta, nPhi) = do
  let rand = getStdRandom (randomR (0, 0.00001))
  noise1 <- rand
  noise2 <- rand
  let [theta, phi] = [vAngle + 2 * (nAngle - vAngle) + noise
                     | (vAngle, nAngle, noise) <-
                       [(vTheta, nTheta, noise1), (vPhi, nPhi, noise2)]]
  return (theta, phi)

diffuse (nTheta, nPhi) = do
  let rand = getStdRandom (randomR (-pi / 2, pi / 2))
  noise1 <- rand
  noise2 <- rand
  return (nTheta + noise1, nPhi + noise2)

rand :: IO Double
rand = getStdRandom (randomR (-pi / 2, pi / 2))
  
