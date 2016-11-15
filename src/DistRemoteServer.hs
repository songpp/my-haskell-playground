{-# LANGUAGE TemplateHaskell #-}
module DistRemoteServer where


import Control.Concurrent               (threadDelay)
import Control.Distributed.Process
import Control.Distributed.Process.Node
import Control.Distributed.Process.Closure
import Control.Monad                    (forever)
import Network.Transport.TCP            (TCPParameters (..), createTransport,
                                         defaultTCPParameters)


sampleTask :: (Int, String) -> Process ()
sampleTask (t, s) = liftIO (threadDelay (t * 1000 * 1000)) >> say s

remotable ['sampleTask]


myRemoteTable :: RemoteTable
myRemoteTable = __remoteTable initRemoteTable

main :: IO ()
main = do
  Right transport <- createTransport "127.0.0.1" "10501" defaultTCPParameters
  node <- newLocalNode transport myRemoteTable
  runProcess node $ do
    us <- getSelfNode
    _ <- spawnLocal $ sampleTask (1 :: Int, "using spawnLocal")
    pid <- spawn us $ $(mkClosure 'sampleTask) (1 :: Int, "using spawn")
    liftIO $ threadDelay (200 * 10000)
