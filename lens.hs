#! /usr/bin/env runhaskell

data FooBar
          = Foo { _x :: [Int], _y :: Int }
            | Bar { _x :: [Int] } deriving (Show)

makeLenses ''FooBar

foo = Foo { _x = [0..5], _y = 6 }
justSix = foo ^? y
x_ = foo ^. x
foo' = foo & x %~ map (+ 1)
foo'' = foo & x .~ [0..9]
foo''' = foo & x . ix 2 .~ 8

