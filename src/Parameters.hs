module Parameters
  ( Object(..)
  , Form(..)
  , distanceFrom
  , distanceFrom'
  , march
  , getNormal
  , objects
  ) where

import           Control.Monad
import           Data.Vector   (Vector, fromList)
import           Debug.Trace
import           Triple
import           Util
