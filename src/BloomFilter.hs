module BloomFilter where


import Data.Word
import Data.Array.ST (STUArray, runSTUArray)
import Data.Array.Unboxed (UArray)

import Control.Monad (liftM)
import Control.Monad.ST (ST, runST)
import qualified Data.Array.Base as ST
import Data.Array.MArray (getBounds, newArray, readArray, writeArray)
import Prelude hiding (elem, length, notElem)


import Murmurhash


data Bloom a = B {
      blmHash  :: a -> [Word32]
    , blmArray :: UArray Word32 Bool
    }

data MutBloom s a = MB {
      mutHash :: a -> [Word32]
    , mutArray :: STUArray s Word32 Bool
    }

create :: (a -> [Word32])        -- ^ hash functions
        -> Int                   -- ^ num bits
        -> Bloom a
create = undefined



new :: (a -> [Word32]) -> Word32 -> ST s (MutBloom s a)
new hash numBits = MB hash `liftM` newArray (0, numBits - 1) False

length :: MutBloom s a -> ST s Word32
length bf = (succ . snd) `liftM` getBounds (mutArray bf)

insert :: MutBloom s a -> a -> ST s ()
insert bf elt = indicies bf elt >>= mapM_ (\bit -> writeArray (mutArray bf) bit True)

indicies :: MutBloom s a -> a -> ST s [Word32]
indicies bf elt = do
  modulus <- length bf
  return $ map (`mod` modulus) (mutHash bf elt)

elem, notElem :: a -> MutBloom s a -> ST s Bool
elem elt bf = indicies bf elt >>= allM (readArray (mutArray bf))

notElem elt bf = not `liftM` elem elt bf

allM :: Monad m => (a -> m Bool) -> [a] -> m Bool
allM p (x : xs) = do
  ok <- p x
  if ok then allM p xs else return False

allM  _ [] = return True


fromList :: (a -> [Word32]) -> Word32 -> [a] -> Bloom a
fromList hash numBits values = runST $ do
    mb <- new hash numBits
    mapM_ (insert mb) values
    B hash `liftM` ST.unsafeFreeze (mutArray mb)




