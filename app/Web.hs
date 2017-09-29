{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Codec.Picture as P
import Conversion (repa3ToText)
import qualified Data.Array.Repa as R
import Data.Array.Repa (Array, DIM3, DIM2, U, D)
import qualified Data.ByteString.Base64
import qualified Data.ByteString.Lazy.Char8
import Data.Conduit (($$), (=$=), Source, Producer)
import Data.Conduit.Internal (zipSources)
import qualified Data.Conduit.List
import Data.Monoid ((<>))
import qualified Data.Text.Encoding
import qualified Data.Text.Lazy as TL
import Lib (traceSource)
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

imgSrcPrefix :: TL.Text
imgSrcPrefix = "data:image/png;base64,"

getImgSrcPrefix :: TL.Text -> TL.Text
getImgSrcPrefix = fst . TL.breakOn imgSrcDelimiter

formatAsImgSrc :: (TL.Text, Array U DIM3 Double) -> TL.Text
formatAsImgSrc (_, array) = imgSrcPrefix <> repa3ToText array

getHomeR :: Handler Html
getHomeR = do
  WS.webSockets $
    sources $$ Data.Conduit.List.map formatAsImgSrc =$= WS.sinkWSText
  defaultLayout $ do
    toWidget $(hamletFile "templates/home.hamlet")
    toWidget $(juliusFile "templates/home.julius")
  where
    sources = zipSources WS.sourceWS traceSource

main :: IO ()
main = warp 3000 App

blackText :: TL.Text
blackText =
  TL.append
    imgSrcPrefix
    "iVBORw0KGgoAAAANSUhEUgAAAAoAAAAKCAIAAAACUFjqAAAADUlEQVR4nGNgGAWkAwABNgABVtF/yAAAAABJRU5ErkJggg=="
