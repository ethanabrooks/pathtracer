{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies,
  OverloadedStrings #-}

module Main where

import qualified Codec.Picture as P
import Conversion (repa3ToText)
import qualified Data.Array.Repa as R
import Data.Array.Repa (Array, DIM3, U)
import qualified Data.ByteString.Base64
import qualified Data.ByteString.Lazy.Char8
import Data.Conduit (($$), (=$), Source)
import Data.Conduit.Internal (zipSources)
import qualified Data.Conduit.List
import Data.Monoid ((<>))
import qualified Data.Text.Encoding
import qualified Data.Text.Lazy as TL
import Lib (traces)
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

imageSource
  :: Monad m
  => Source (WS.WebSocketsT Handler) (m (Array U DIM3 Double))
imageSource = Data.Conduit.List.sourceList traces

imgSrcPrefix :: TL.Text
imgSrcPrefix = "data:image/png;base64,"

blackText :: TL.Text
blackText =
  TL.append
    imgSrcPrefix
    "iVBORw0KGgoAAAANSUhEUgAAAAoAAAAKCAIAAAACUFjqAAAADUlEQVR4nGNgGAWkAwABNgABVtF/yAAAAABJRU5ErkJggg=="

getImgSrcPrefix :: TL.Text -> TL.Text
getImgSrcPrefix = fst . (TL.breakOn imgSrcDelimiter)

formatAsImgSrc
  :: Monad m
  => (TL.Text, m (Array U DIM3 Double)) -> m TL.Text
formatAsImgSrc (_, arrayM) = do
  array <- arrayM
  return $ imgSrcPrefix <> repa3ToText array

getHomeR :: Handler Html
getHomeR = do
  WS.webSockets $
    zipSources WS.sourceWS imageSource $$ Data.Conduit.List.mapM formatAsImgSrc =$
    WS.sinkWSText
  defaultLayout $ do
    toWidget $(hamletFile "templates/home.hamlet")
    toWidget $(juliusFile "templates/home.julius")

main :: IO ()
main = warp 3000 App
