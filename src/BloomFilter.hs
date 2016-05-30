module BloomFilter where

import Data.Array.Unboxed
import Data.Word

import Murmurhash




data BloomFilter a = B {
      hashes    :: !( a -> [Word32] )
    , bits      :: ! Int
    , bitArray  :: !(UArray Int Word32)
    }


create :: (a -> [Word32])        -- ^ hash functions
        -> Int                   -- ^ num bits
        -> BloomFilter a
create = undefined


