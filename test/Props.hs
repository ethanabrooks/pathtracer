{-# LANGUAGE TemplateHaskell #-}

import qualified Test.QuickCheck as T
import Test.QuickCheck (quickCheckAll, (==>))
import Triple (Triple (..), norm2, normalize, tSum)

(~=) :: (Num a, Ord a, Fractional a) => a -> a -> Bool
a ~= b = (a - b) * (a - b) < 0.00001

prop_norm2 = norm2 (Triple 3 4 5) ~= 7.0710678118654755
prop_normalize triple = tSum triple > 10**(-6) ==> norm2 (normalize triple) ~= 1
prop_normalize2 = norm2 (normalize $ Triple 0 0 0) == 0

return []
runTests = $quickCheckAll

main = do runTests
