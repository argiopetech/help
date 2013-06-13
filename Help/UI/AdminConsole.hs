{-# LANGUAGE NoImplicitPrelude, UnicodeSyntax #-}
module Help.UI.AdminConsole where

import Help.Imports
import Help.Settings

import Control.Lens
import System.Remote.Monitoring
import qualified Data.ByteString.Char8 as BC

adminConsole ∷ Settings → IO ()
adminConsole s = void $ forkServer (BC.pack $ adminHost ^$ s) (adminPort ^$ s)
