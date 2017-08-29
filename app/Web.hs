{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies,
  FlexibleContexts, OverloadedStrings #-}

module Main where

import qualified Codec.Picture as P
import Control.Arrow
import Conversion (imageToText, repaToImage, toImage)
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
import Triple (Triple)
import Util (fromTripleArray, toTripleArray, flatten, reshape)
import Yesod.Core
import qualified Yesod.WebSockets as WS

data App =
  App

instance Yesod App

mkYesod "App" [parseRoutes| / HomeR GET |]

imgSrcDelimiter :: TL.Text
imgSrcDelimiter = ","

f :: Array D DIM3 Double -> Array D DIM3 Double
f =
  fromTripleArray .
  (reshape [Params.imgHeight, Params.imgWidth]) .
  (traceCanvas Params.maxBounces) . flatten . toTripleArray

imageSource :: Source (WS.WebSocketsT Handler) (Array D DIM3 Double)
imageSource = Data.Conduit.List.iterate f black

imageSource' :: Source (WS.WebSocketsT Handler) (Array D DIM1 (Triple Double))
imageSource' = Data.Conduit.List.iterate (traceCanvas Params.maxBounces) black'

black :: Array D DIM3 Double
black = R.fromFunction (Z :. Params.imgHeight :. Params.imgWidth :. 3) $ const 0

black' :: Array D DIM1 (Triple Double)
black' = R.fromFunction (Z :. Params.imgHeight * Params.imgWidth) $ const 0

blackByteString :: TL.Text
blackByteString =
  "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAoAAAAKCAIAAAACUFjqAAAADUlEQVR4nGNgGAWkAwABNgABVtF/yAAAAABJRU5ErkJggg=="

getImgSrcPrefix :: TL.Text -> TL.Text
getImgSrcPrefix = fst . (TL.breakOn imgSrcDelimiter)

combineStreams :: (TL.Text, Array D DIM3 Double) -> TL.Text
combineStreams =
  (getImgSrcPrefix *** (imageToText . repaToImage)) >>>
  (\(a, b) -> a <> imgSrcDelimiter <> b)

combineStreams' :: (TL.Text, Array D DIM1 (Triple Double)) -> TL.Text
combineStreams' =
  (getImgSrcPrefix ***
   (imageToText . toImage . (reshape [Params.imgHeight, Params.imgWidth]))) >>>
  (\(a, b) -> a <> imgSrcDelimiter <> b)

getHomeR :: Handler Html
getHomeR = do
  WS.webSockets $
    zipSources WS.sourceWS imageSource' $$
    (Data.Conduit.List.map combineStreams') =$
    WS.sinkWSText
  defaultLayout $ do
    toWidget $(hamletFile "templates/home.hamlet")
    toWidget $(juliusFile "templates/home.julius")

main :: IO ()
main = warp 3000 App
