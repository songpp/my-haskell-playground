{-# LANGUAGE GADTs #-}
module Main where

import Control.Concurrent
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import Data.IORef
import Data.Vector as V
import JavaClassParser
import Semaphore
import Text.Printf (printf)

import Data.Number.Fixed

{-
main :: IO ()
main = do
  putStrLn "Playground"

mstr = "String， wahaha\n\t\
        \\"line 2\"\n"
-}

maybePrint :: IORef Bool -> IORef Bool -> IO ()
maybePrint myRef yourRef = do
   writeIORef myRef True
   yourVal <- readIORef yourRef
   unless yourVal $ putStrLn "critical section"

main :: IO ()
main = do
   r1 <- newIORef False
   r2 <- newIORef False
   forkIO $ maybePrint r1 r2
   forkIO $ maybePrint r2 r1
   threadDelay 1000000
