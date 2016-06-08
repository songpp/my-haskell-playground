{-# LANGUAGE BangPatterns #-}
module Semaphore
    (
      Semaphore
    , Timeout
    , newSemaphore
    , acquire
    , release
    ) where


import Control.Applicative
import Data.IORef

data Semaphore = MkSemaphore (IORef Int)
newtype Timeout = Timeout Integer

newSemaphore :: Int -> IO Semaphore
newSemaphore n = return . MkSemaphore =<< newIORef n

acquire :: Semaphore  -> IO Bool
acquire (MkSemaphore holder) =
  atomicModifyIORef' holder $ \n ->
    if n == 0
    then (n, False)
    else let !z = n - 1 in (z, True)

release :: Semaphore -> IO ()
release (MkSemaphore holder) =
  atomicModifyIORef' holder $ \n ->
    let !z = n + 1 in (z, ())
