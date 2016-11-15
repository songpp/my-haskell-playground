module Phantom where

import Data.IORef


data ReadOnly
data ReadWrite

newtype SafeCfg c v = Config (IORef v)

newConfig :: v -> IO (SafeCfg ReadWrite v)
newConfig v = Config `fmap` newIORef v

readOnly :: SafeCfg c v -> SafeCfg ReadOnly v
readOnly (Config ref) = Config ref

readCfg :: SafeCfg c v -> IO v
readCfg (Config ref) = readIORef ref

writeCfg :: SafeCfg ReadWrite v -> v -> IO ()
writeCfg (Config ref) v = writeIORef ref v

