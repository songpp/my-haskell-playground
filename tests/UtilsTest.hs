module UtilsTest where

import Murmurhash
import Data.ByteString
import Data.ByteString.Lazy.Char8 as L

import Test.Hspec

main :: IO ()
main = hspec $
  describe "toBinary" $
    it "1 should be 1" $
      murmur3 147 (L.toStrict $ L.pack "Hello") `shouldBe` 1120696757