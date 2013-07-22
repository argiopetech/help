{-# LANGUAGE NoImplicitPrelude, UnicodeSyntax #-}
module Help.UI.AdminConsole where

import Help.Imports
import Help.Settings

import Control.Concurrent
import System.Remote.Monitoring
import qualified Data.ByteString.Char8 as BC

import Control.Lens.Getter ((^.))

adminConsole ∷ Settings → IO ()
adminConsole s = do
    _ ← forkServer (BC.pack $ s^.adminHost) (s^.adminPort)
    forever $ threadDelay 1000000
