{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TemplateHaskell #-}
module Monads where

import Control.Lens.TH
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans
import Control.Monad.Writer
import Data.Default

import GHC.Generics
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))


listDirectory :: FilePath -> IO [FilePath]
listDirectory = fmap (filter notDots) . getDirectoryContents
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
  _appKey   :: String,
  _logLevel :: String
} deriving (Show, Eq)

makeLenses ''Config
