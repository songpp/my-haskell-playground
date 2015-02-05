module Conduits where

import           Data.Conduit

import           Control.Monad.Trans.Resource
import           Data.ByteString              ( ByteString )
import qualified Data.ByteString.Char8        as BC
import           Data.Char                    (toUpper)
import qualified Data.Conduit.Binary          as CB
import qualified Data.Conduit.List            as CL


doCopy :: IO ()
doCopy = runResourceT
      $ CB.sourceFile "src/Conduits.hs"
      $$ CB.sinkFile "output.txt"

