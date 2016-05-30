module Main where

import Criterion.Main
import Data.Word
import Murmurhash
import Data.ByteString
import Data.ByteString.Lazy.Char8 as L

main :: IO ()
main = defaultMain [
  bgroup "Murmurhash3"
      [
        bench "1"  $ whnf (murmur3 (147 :: Word32)) (L.toStrict $ L.pack "Hello")
      ]
  ]
