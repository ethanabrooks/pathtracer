{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Array.Repa as R
import Data.Array.Repa (Array, DIM1, (+^), D, Z(..), (:.)(..), (!))
import Data.Conduit (($$), (=$=), Source, Producer, Conduit)
import qualified Data.Conduit.List
import qualified Data.Text.Lazy as TL
import Debug.Trace
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
    i = array ! (Z :. 0) -- use the first element in the array as the random seed
    array' =
      R.fromListUnboxed (Z :. n) . take n $ Random.randoms (Random.mkStdGen i) -- random array

source
  :: Monad m
  => Source m Int
source =
  Data.Conduit.List.iterate addLargeRandomArrays zeros =$= -- repeatedly add large random arrays
  Data.Conduit.List.mapM sum -- reduce each new array to its sum
  where
    zeros = R.fromFunction (Z :. n) $ const 0 -- all zeros
    sum array = (! Z) <$> R.sumP array -- reduction

getHomeR :: Handler Html
getHomeR = do
  WS.webSockets $
    source $$ Data.Conduit.List.map (TL.pack . show) =$= WS.sinkWSText
  defaultLayout $ do
    html
    toWidget js

main :: IO ()
main = warp 3000 App

html =
  [whamlet|
$doctype 5
<html>
    <head>
        <title>My Site
    <body>
        <p id=time>
          |]

js =
  [julius|
var conn = new WebSocket("ws://localhost:3000/");

time = document.getElementById('time');
count = 0;
tick = newTick();

conn.onmessage = function(e) {
    // print out duration of computation (in seconds) for each iteration
    time.innerHTML += "<br>" + count + ": " + secondsSince(tick).toString();
    tick = newTick();
    count += 1;
};
function newTick() {
  return new Date().getTime();
}
function secondsSince(tick) {
  return (newTick() - tick) / 1000;
}
|]
