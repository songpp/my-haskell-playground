{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module DateTime where

import Control.Lens
import Data.Aeson
import Data.Aeson.Types
import Data.Time (NominalDiffTime, UTCTime)
import qualified Data.Time as Time
import Data.Time.Clock
import Data.Typeable
import GHC.Generics
import System.IO.Unsafe

-- :l src/DateTime.hs
-- import qualified Data.Text.Lazy.IO as T
-- import qualified Data.Text.Lazy.Encoding as T

tomorrow = do
  UTCTime day time <- Time.getCurrentTime
  return (UTCTime (Time.addDays 1 day) time)


{-# NOINLINE getCurrentUTC #-}
getCurrentUTC = unsafePerformIO  Time.getCurrentTime


data Event = Event {
    _id         :: String,
    _timestamps :: UTCTime
    } deriving (Show, Eq, Typeable, Generic)

makeLenses ''Event


instance ToJSON Event where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 1 }


instance FromJSON Event where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }
