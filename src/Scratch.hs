module Scratch where

import Control.Lens

data FooBar a
  = Foo { _x :: [Int], _y :: a }
  | Bar { _x :: [Int] }
makeLenses ''FooBar
