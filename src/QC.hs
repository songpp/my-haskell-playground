{-# LANGUAGE NoImplicitPrelude #-}
module QC where

import Data.List       hiding (head, minimum)
import Prelude         hiding (head, minimum)
import System.Random   (mkStdGen)
import Test.QuickCheck


qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) = qsort lhs ++ [x] ++ qsort rhs
    where lhs = filter (< x) xs
          rhs = filter (>= x) xs

prop_idempotent :: [a] -> Bool
prop_idempotent xs = qsort (qsort xs) == qsort xs


head       :: [a] -> a
head (x:_) = x
head []    = error "Prelude.head: empty list"

minimum    :: (Ord a) => [a] -> a
minimum [] =  error "Prelude.minimum: empty list"
minimum xs =  foldl1 min xs

prop_minimum' xs = not (null xs) ==> head (qsort xs) == minimum xs

prop_stupid :: [a] -> Bool
prop_stupid xs = forAll (elements xs) $ \c -> c == 0
