{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}
module Monads where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Lens.TH
import Data.Default

import System.Directory
import System.FilePath      ((</>))
import GHC.Generics


listDirectory :: FilePath -> IO [FilePath]
listDirectory = liftM (filter notDots) . getDirectoryContents
  where
    notDots p = p /= "." && p /= ".."


countEntries :: FilePath -> WriterT [(FilePath, Int)] IO ()
countEntries p = do
    contents <- liftIO . listDirectory $ p
    tell [(p, length contents)]
    forM_ contents $ \name -> do
      let newName = p </> name
      isDir <- liftIO . doesDirectoryExist $ newName
      when isDir $ countEntries newName


data Config a = Config {
  _appKey :: String,
  _logLevel :: String
} deriving (Show, Eq)

makeLenses ''Config