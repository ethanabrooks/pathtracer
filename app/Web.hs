{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies,
  FlexibleContexts, OverloadedStrings #-}

module Main where

import qualified Codec.Picture as P
import Control.Arrow
import Conversion (imageToText, repa3ToImage, repa1ToText)
import qualified Data.Array.Repa as R
import Data.Array.Repa ((:.)(..), Array, D, DIM1, DIM3, Z(..), (!))
import qualified Data.ByteString.Base64
import qualified Data.ByteString.Lazy.Char8
import Data.Conduit (($$), (=$), Source, Conduit)
import Data.Conduit.Internal (zipSources)
import qualified Data.Conduit.List
import Data.Fixed (mod')
import Data.Monoid ((<>))
import qualified Data.Text.Encoding
import qualified Data.Text.Lazy as TL
import Lib (traceCanvas)
import qualified Params
import Text.Hamlet (hamletFile)
import Text.Julius (juliusFile)
import Triple (Triple, Vec3)
import Util
       (fromTripleArray, toTripleArray, flatten, reshape, blackCanvas)
import Yesod.Core
import qualified Yesod.WebSockets as WS

data App =
  App

instance Yesod App

mkYesod "App" [parseRoutes| / HomeR GET |]

imgSrcDelimiter :: TL.Text
imgSrcDelimiter = ","

imageSource :: Source (WS.WebSocketsT Handler) (Int, Array D DIM1 Vec3)
imageSource = Data.Conduit.List.iterate traceCanvas $ (0, flatten blackCanvas)

rendered :: Array D DIM1 Vec3
rendered = snd $ iterate traceCanvas (0, flatten blackCanvas) !! 10

renderedText :: TL.Text
renderedText = formatAsImgSrc (blackText, rendered)

imgSrcPrefix :: TL.Text
imgSrcPrefix = "data:image/png;base64,"

blackText :: TL.Text
blackText =
  TL.append
    imgSrcPrefix
    "iVBORw0KGgoAAAANSUhEUgAAAAoAAAAKCAIAAAACUFjqAAAADUlEQVR4nGNgGAWkAwABNgABVtF/yAAAAABJRU5ErkJggg=="

getImgSrcPrefix :: TL.Text -> TL.Text
getImgSrcPrefix = fst . (TL.breakOn imgSrcDelimiter)

formatAsImgSrc :: (TL.Text, Array D DIM1 Vec3) -> TL.Text
formatAsImgSrc = (imgSrcPrefix <>) . repa1ToText . snd

discardIteration :: (TL.Text, (Int, Array D DIM1 Vec3))
                 -> (TL.Text, Array D DIM1 Vec3)
discardIteration (text, (iteration, array)) = (text, array)

getHomeR :: Handler Html
getHomeR = do
  WS.webSockets $
    zipSources WS.sourceWS imageSource $$
    (Data.Conduit.List.map $ formatAsImgSrc . discardIteration) =$
    WS.sinkWSText
  defaultLayout $ do
    toWidget $(hamletFile "templates/home.hamlet")
    toWidget $(juliusFile "templates/home.julius")

main :: IO ()
main = warp 3000 App
