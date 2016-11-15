
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module DistServer where


import Network.Transport.TCP (createTransport, defaultTCPParameters, TCPParameters(..))
import Control.Distributed.Process
import Control.Distributed.Process.Node
import Control.Concurrent (threadDelay)
import Control.Monad (forever)


deriving instance Show TCPParameters

main = do
  Right t <- createTransport "127.0.0.1" "10501" defaultTCPParameters
  node <- newLocalNode t initRemoteTable
  runProcess node $ do
    echoPid <- spawnLocal . forever $ receiveWait [match logMessage, match replyBack]

    say "send me messages !"
    send echoPid "hello"

    self <- getSelfPid
    send echoPid (self, "hello")

    m <- expectTimeout (100 * 10000)
    case m of
      Nothing -> die "nothing came back!"
      Just s -> say $ "got " ++ s ++ " back!"
    liftIO $ threadDelay (200 * 10000)


replyBack :: (ProcessId, String) -> Process ()
replyBack (sender, msg) = send sender msg


logMessage :: String -> Process ()
logMessage msg = say $ "handling " ++ msg
