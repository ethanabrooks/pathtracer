{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies,
  FlexibleContexts, OverloadedStrings #-}

module Main where

import qualified Codec.Picture as P
import Control.Arrow
import qualified Data.Array.Repa as R
import Data.Array.Repa ((:.)(..), Array, D, DIM3, Z(..), (!))
import qualified Data.ByteString.Base64
import qualified Data.ByteString.Lazy.Char8
import Data.Conduit (($$), (=$), Source)
import Data.Conduit.Internal (zipSources)
import qualified Data.Conduit.List
import Data.Fixed (mod')
import Data.Monoid ((<>))
import qualified Data.Text.Encoding
import qualified Data.Text.Lazy as TL
import Text.Hamlet (hamletFile)
import Text.Julius (juliusFile)
import Yesod.Core
import qualified Yesod.WebSockets as WS

data App =
  App

instance Yesod App

mkYesod "App" [parseRoutes| / HomeR GET |]

imgSrcDelimiter :: TL.Text
imgSrcDelimiter = ","

height :: Int
height = 100

width :: Int
width = 100

white :: Array D DIM3 Double
white = R.fromFunction (Z :. height :. width :. 3) $ const 255

black :: Array D DIM3 Double
black = R.fromFunction (Z :. height :. width :. 3) $ const 0

f :: Array D DIM3 Double -> Array D DIM3 Double
f = R.map ((`mod'` 255) . (+ 10))

imageSource :: Source (WS.WebSocketsT Handler) (Array D DIM3 Double)
imageSource = Data.Conduit.List.iterate f black

blackByteString :: TL.Text
blackByteString =
  "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAoAAAAKCAIAAAACUFjqAAAADUlEQVR4nGNgGAWkAwABNgABVtF/yAAAAABJRU5ErkJggg=="

getImgSrcPrefix :: TL.Text -> TL.Text
getImgSrcPrefix = fst . (TL.breakOn imgSrcDelimiter)

imageToText :: P.Image P.PixelRGB8 -> TL.Text
imageToText =
  TL.fromStrict .
  Data.Text.Encoding.decodeUtf8 .
  Data.ByteString.Base64.encode .
  Data.ByteString.Lazy.Char8.toStrict . P.encodePng

repaToImage
  :: (R.Source r Double)
  => Array r DIM3 Double -> P.Image P.PixelRGB8
repaToImage canvas = P.generateImage fromCoords height width
  where
    fromCoords i j =
      let [r, g, b] = round <$> [canvas ! (Z :. i :. j :. k) | k <- [0 .. 2]]
      in P.PixelRGB8 r g b

combineStreams :: (TL.Text, Array D DIM3 Double) -> TL.Text
combineStreams =
  (getImgSrcPrefix *** (imageToText . repaToImage)) >>>
  (\(a, b) -> a <> imgSrcDelimiter <> b)

getHomeR :: Handler Html
getHomeR = do
  WS.webSockets $
    zipSources WS.sourceWS imageSource $$ (Data.Conduit.List.map combineStreams) =$
    WS.sinkWSText
  defaultLayout $ do
    toWidget $(hamletFile "templates/home.hamlet")
    toWidget $(juliusFile "templates/home.julius")

main :: IO ()
main = warp 3000 App
