{-# LANGUAGE TemplateHaskell #-}

module A where

import Test.QuickCheck

prop_a = 1 == 0

return []
check = $quickCheckAll
