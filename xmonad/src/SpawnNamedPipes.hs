{-# LANGUAGE DeriveDataTypeable #-}

module SpawnNamedPipes
  ( spawnNamedPipes
  , getNamedPipes
  ) where

import           XMonad.Core                 ( ExtensionClass(initialValue), X)
import           XMonad.Util.Run             ( spawnPipe )
import qualified XMonad.Util.ExtensibleState as XS

import           Control.Monad               ( unless )
import           Data.Map                    ( Map )
import qualified Data.Map                    as Map
import           Data.Typeable               ( Typeable )
import           System.IO                   ( Handle )

data NamedPipes = NamedPipes { pipesMap :: Map String [Handle] }
    deriving (Show, Typeable)

instance ExtensionClass NamedPipes where
    initialValue = NamedPipes Map.empty

spawnNamedPipes :: [String] -> String -> X ()
spawnNamedPipes cmds name = do
    b <- XS.gets (Map.member name . pipesMap)
    unless b $ do
        hs <- mapM spawnPipe cmds
        XS.modify (NamedPipes . Map.insert name hs . pipesMap)

getNamedPipes :: String -> X (Maybe [Handle])
getNamedPipes name = XS.gets (Map.lookup name . pipesMap)
