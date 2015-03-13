{-# LANGUAGE GADTs #-}
module Main where

import JavaClassParser
import Data.Vector as V
import Text.Printf (printf)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L

main :: IO ()
main = do
  putStrLn "Playground"

mstr = "这是一条多行String， wahaha\n\t\
        \\"第二行开始\"\n"
