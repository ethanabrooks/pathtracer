{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators   #-}

module Main (main) where

import Control.Lens
import Data.List
import Codec.Picture
import Control.Monad
import Control.Monad.ST
import Data.Array.Repa (Array, DIM1, DIM2, U, D, Z (..), (:.)(..), (!))
import System.Environment (getArgs)
import System.FilePath (replaceExtension)
import qualified Codec.Picture.Types as M
import qualified Data.Array.Repa     as R -- for Repa
import Lib (Vec3, Rays, emptyRays, raysFromCam, _num, vec3ToImage)

type RGB8 = (Pixel8, Pixel8, Pixel8)



-- | Get image with true color pixels from manifest Repa array.
toImage :: Array U DIM2 RGB8 -> Image PixelRGB8
toImage a = generateImage gen width height
  where Z :. width :. height = R.extent a
        gen x y = let (r,g,b) = a ! (Z :. x :. y)
                  in PixelRGB8 r g b


-- | Paramters
imgHeight = 1200
imgWidth  = 1200
cameraDepth = 10 

numRays = imgHeight * imgWidth

imgDimensions :: DIM2
imgDimensions = (Z :. imgHeight :. imgWidth)



numIters :: Int
numIters = 100

objects = []

type ImageArray = Array D DIM2 RGB8

main :: IO ()
main = do
  img    <- R.computeUnboxedP repaImg
  (savePngImage "image.png" . ImageRGB8 . toImage) img

repaImg :: ImageArray
repaImg = R.fromFunction imgDimensions originalFnc

newRGB :: Int -> Int -> Int -> RGB8
newRGB x y z = (pixel x, pixel y, pixel z)
  where pixel = fromIntegral . min 0xff

originalFnc :: DIM2 -> RGB8
originalFnc (Z :. x :. y) =
  let (q, r) = x `quotRem` max 3 y
  in  newRGB q r (q + r + 30)

blankCanvas = R.fromFunction imgDimensions $ const 0 :: Vec3

repaImg' = vec3ToImage $ iterate rayTrace blankCanvas !! numIters :: ImageArray

rayTrace :: Vec3 -> Vec3
rayTrace canvas = canvas'
  where (_, canvas') = until allTerminal bounce (raysFromCam, canvas)
        allTerminal (nonTerminalRays, _)  = _num nonTerminalRays == 0

bounce :: (Rays, Vec3) -> (Rays, Vec3)
bounce = id -- TODO
  
bounceRays :: Rays -> Rays
bounceRays rays = rays



-- rayTrace (rays, canvas, lengths) = foldl
--     (\(rays, canvas, lengths) object ->
--       let distanceToObject = distanceBetween object rays
          
--       in  ( object `reflect` rays
--           , colorWithTerminalRays, newDistances))
--     (raysFromCam, blankCanvas, infDistance) objects

-- reflect :: Elements -> Object -> Elements
-- reflect (rays, canvas, lengths) object = (newRays, newCanvas, distances)
--   where distanceToObject  = distanceBetween object rays
--         intersectionPoint = intersection object rays
--         closer            = 
