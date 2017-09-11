{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import qualified Codec.Picture as P
import Conversion (repa3ToText)
import qualified Data.Array.Repa as R
import Data.Array.Repa
       (Array, DIM3, DIM2, DIM1, DIM0, (+^), U, D, Z(..), (:.)(..), (!))
import qualified Data.ByteString.Base64
import qualified Data.ByteString.Lazy.Char8
import Data.Conduit (($$), (=$=), Source, Producer, Conduit)
import Data.Conduit.Internal (zipSources)
import qualified Data.Conduit.List
import Data.Monoid ((<>))
import qualified Data.Text.Encoding
import qualified Data.Text.Lazy as TL
import Lib (traceSource)
import qualified System.Random as Random
import Text.Hamlet (hamletFile)
import Text.Julius (juliusFile)
import Yesod.Core
import qualified Yesod.WebSockets as WS

data App =
  App

instance Yesod App

mkYesod "App" [parseRoutes| / HomeR GET |]

n = round 1e6 :: Int

addLargeRandomArrays :: Array D DIM1 Int -> Array D DIM1 Int
addLargeRandomArrays array = array +^ array'
  where
    i = array ! (Z :. 0)
    array' =
      R.fromListUnboxed (Z :. n) . take n $ Random.randoms (Random.mkStdGen i)

source
  :: Monad m
  => Source m Int
source =
  (Data.Conduit.List.iterate addLargeRandomArrays zeros) =$=
  (Data.Conduit.List.mapM sum)
  where
    zeros = R.fromFunction (Z :. n) $ const 0
    sum array = R.sumP array >>= return . (! Z)

getHomeR :: Handler Html
getHomeR = do
  WS.webSockets $
    source $$ Data.Conduit.List.map (TL.pack . show) =$= WS.sinkWSText
  defaultLayout $ do
    toWidget $(hamletFile "templates/home.hamlet")
    toWidget $(juliusFile "templates/home.julius")

main :: IO ()
main = warp 3000 App
