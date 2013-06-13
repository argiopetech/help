{-# LANGUAGE NoImplicitPrelude, UnicodeSyntax #-}
module Help.UI.AdminConsole where

import Help.Imports
import Help.Settings

import Control.Concurrent
import Control.Lens
import System.Remote.Monitoring
import qualified Data.ByteString.Char8 as BC

adminConsole ∷ Settings → IO ()
adminConsole s = do
    _ ← forkServer (BC.pack $ adminHost ^$ s) (adminPort ^$ s)
    forever $ threadDelay 1000000
