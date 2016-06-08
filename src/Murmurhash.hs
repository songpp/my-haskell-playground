{-# LANGUAGE BangPatterns             #-}
{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings        #-}
module Murmurhash where

import Control.Monad (replicateM)


import Data.ByteString (ByteString, packCString, useAsCStringLen)
import qualified Data.ByteString as BS (ByteString, append, drop, length, replicate)
import Data.Serialize.Get (getWord32le, runGet)

import Data.Bits (rotateL, shiftR, xor)
import Data.List (foldl')
import Data.Word (Word32)
import Foreign.C.String (CString)
import Foreign.C.Types (CChar (..), CInt (..), CSize (..), CUInt (..))
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (allocaArray, withArrayLen)
import Foreign.Ptr (Ptr, castPtr, nullPtr, plusPtr)
import Foreign.Storable (Storable, peek, poke, sizeOf)
import System.IO.Unsafe (unsafePerformIO)

import Data.Memory.ExtendedWords

#include "HsBaseConfig.h"

foreign import ccall unsafe "murmur3.h MurmurHash3_x86_32" murmur3X86Hash32
    :: Ptr () -> CInt -> CUInt -> Ptr Word32 -> IO ()

foreign import ccall unsafe "murmur3.h MurmurHash3_x86_128" murmur3X86Hash128
    :: Ptr () -> CInt -> CUInt -> Ptr Word128 -> IO ()

foreign import ccall unsafe "murmur3.h MurmurHash3_x64_128" murmur3X64Hash128
    :: Ptr () -> CInt -> CUInt -> Ptr Word128 -> IO ()


type Seed = Word32


murmur3x86'32 :: ByteString -> Seed -> Word32
murmur3x86'32 value seed = unsafePerformIO . alloca $ \out ->
    useAsCStringLen value $ \(ptr, len) -> do
        murmur3X86Hash32 (castPtr ptr) (fromIntegral len) (fromIntegral seed) (castPtr out)
        peek out

murmur3x86'128 :: ByteString -> Seed -> Word128
murmur3x86'128 = undefined

murmur3x64'128 :: ByteString -> Seed -> Word128
murmur3x64'128 = undefined


callMurmur3Hash32 :: IO Word32
callMurmur3Hash32 = alloca $ \out ->
    useAsCStringLen "Hello, world!" $ \(c, size) -> do
        murmur3X86Hash32 (castPtr c) (fromIntegral size) 4321 (castPtr out)
        peek out


-- | MurmurHash3 (x86_32). For more details, see
-- <http://code.google.com/p/smhasher/source/browse/trunk/MurmurHash3.cpp>
murmur3 :: Word32         -- ^ Seed value
        -> BS.ByteString  -- ^ Strict bytestring data to hash
        -> Word32         -- ^ MurmurHash3 result
murmur3 nHashSeed bs =
    h8
  where
    !dataLength = BS.length bs
    -- Block and tail sizes
    !nBlocks = dataLength `quot` 4
    !nTail   = dataLength `rem` 4
    -- Data objects
    Right blocks  = runGet (replicateM nBlocks getWord32le) bs
    bsTail  = BS.drop (nBlocks*4) bs `BS.append` BS.replicate (4-nTail) 0
    -- Body
    !h1   = foldl' mix nHashSeed blocks
    -- Tail
    Right !t1   = runGet getWord32le bsTail
    !t2   = t1 * c1
    !t3   = t2 `rotateL` 15
    !t4   = t3 * c2
    !h2   = h1 `xor` t4
    -- Finalization
    !h3   = h2 `xor` fromIntegral dataLength
    !h4   = h3 `xor` (h3 `shiftR` 16)
    !h5   = h4 * 0x85ebca6b
    !h6   = h5 `xor` (h5 `shiftR` 13)
    !h7   = h6 * 0xc2b2ae35
    !h8   = h7 `xor` (h7 `shiftR` 16)
    -- Mix function
    mix !r1 !k1 = r4
      where
        !k2 = k1 * c1
        !k3 = k2 `rotateL` 15
        !k4 = k3 * c2
        !r2 = r1 `xor` k4
        !r3 = r2 `rotateL` 13
        !r4 = r3*5 + 0xe6546b64
    -- Constants
    c1 = 0xcc9e2d51
    c2 = 0x1b873593
