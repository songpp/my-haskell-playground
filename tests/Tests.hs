module Main where

import           Test.Framework                       (defaultMain, testGroup)
import           Test.Framework.Providers.QuickCheck2

main :: IO ()
main = defaultMain tests


qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) = qsort lhs ++ [x] ++ qsort rhs
    where lhs = filter (< x) xs
          rhs = filter (>= x) xs

prop_idempotent :: Ord a => [a] -> Bool
prop_idempotent xs = qsort (qsort xs) == qsort xs


tests = [
    testGroup "qsort" [
      testProperty "should be idempotent" (prop_idempotent :: [Integer] -> Bool)
    ]
  ]
