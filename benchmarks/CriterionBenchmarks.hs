{-# LANGUAGE OverloadedStrings #-}
module Main where

import Criterion.Main
import Data.ByteString
import Data.ByteString.Lazy.Char8 as L
import Data.Word
import Murmurhash

main :: IO ()
main = defaultMain [
  bgroup "Murmurhash3"
      [
        bench "mm3 haskell"  $ whnf (murmur3 1234) "Hello, world!"
      , bench "mm32 binding" $ whnf (murmur3x86'32 "Hello, world!") 1234
      ]
  ]


{--
benchmarking Murmurhash3/mm3 haskell
time                 154.0 ns   (151.1 ns .. 157.1 ns)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 151.7 ns   (150.4 ns .. 153.5 ns)
std dev              5.293 ns   (4.151 ns .. 7.160 ns)
variance introduced by outliers: 53% (severely inflated)

benchmarking Murmurhash3/mm32 binding
time                 43.04 ns   (42.49 ns .. 43.73 ns)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 44.23 ns   (43.66 ns .. 44.95 ns)
std dev              2.188 ns   (1.690 ns .. 2.973 ns)
variance introduced by outliers: 71% (severely inflated)

Benchmark bench: FINISH
--}
