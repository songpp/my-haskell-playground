
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}

module MyTypeFamilies where


import Data.IORef
import Control.Applicative
import Data.Foldable (forM_)
import Control.Concurrent.STM
import Control.Concurrent.MVar


class IOStore store where
  newIO :: a -> IO (store a)
  getIO :: store a -> IO a
  putIO :: store a -> a -> IO ()


instance IOStore MVar where
  newIO = newMVar
  getIO = readMVar
  putIO mvar a = modifyMVar_ mvar (return . const a)


instance IOStore IORef where
  newIO = newIORef
  getIO = readIORef
  putIO ref a = modifyIORef' ref (const a)


type Present = String
storePresentsIO :: IOStore s => [Present] -> IO (s [Present])
storePresentsIO ps = do
  store <- newIO []
  forM_ ps $ \x -> do
    old <- getIO store
    putIO store (x : old)
  return store


class Store store where
  type StoreMonad store :: * -> *
  new :: a -> (StoreMonad store) (store a)
  get :: store a -> (StoreMonad store) a
  put :: store a -> a -> (StoreMonad store) ()

instance Store IORef where
  type StoreMonad IORef = IO
  new = newIORef
  get = readIORef
  put ref a = modifyIORef ref (const a)
