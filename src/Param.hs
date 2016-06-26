-- @Author: spp
-- @Date:   2014-09-26 17:16:53
-- @Last Modified by:   spp
-- @Last Modified time: 2014-09-26 17:21:58

{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
module Params(
    Params,
    Param,
    mkParam,
    singleton,
    put,
    lookup,

    Default(def),

    Port(..),
    TempDir(..),

    defaultParams
    ) where

-- existentials

import Data.Map (Map)
import qualified Data.Map as M
import Data.Proxy
import Data.Typeable
import Prelude hiding (lookup)
import System.FilePath

class Default a where
    def :: a

newtype Port = Port Int deriving (Show, Eq, Typeable)
newtype TempDir = TempDir FilePath deriving (Show, Eq, Typeable)


instance Default Port where
    def = Port 8080

instance Default TempDir where
    def = TempDir "/tmp"

data Param = forall a . (Default a, Show a, Typeable a) => Param a (Proxy a)

mkParam :: (Default a, Typeable a, Show a) => a -> Param
mkParam a = Param a (Proxy :: Proxy a)

instance Show Param where
    show (Param a p) = show a

newtype Params = MkMap { unMap :: Map TypeRep Param } deriving (Show)

singleton :: Param  -> Params
singleton p@(Param _ v) = MkMap $ M.singleton (typeOf v) p

insert :: Params -> Param -> Params
insert ps p@(Param _ v) = MkMap $ M.insert (typeOf v) p (unMap ps)

put :: (Default a, Typeable a, Show a) => Params -> a -> Params
put ps a = insert ps (mkParam a)

lookup' :: (Default a, Typeable a) => Proxy a -> Params -> a
lookup' proxy ps
    | Just (Param v p) <- M.lookup (typeOf proxy) (unMap ps), Just r <- cast v = r
    | otherwise = def


lookup :: (Default a, Typeable a) => Params -> a
lookup = lookup' (Proxy :: Proxy a)

confs ::  [Param]
confs = [mkParam (Port 8080), mkParam (TempDir "/var/tmp")]

defaultParams = let m = singleton (mkParam $ Port 80)
                in foldr (flip insert) m confs

defaultPort :: Port
defaultPort = lookup defaultParams
