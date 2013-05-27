{-# LANGUAGE NoImplicitPrelude, UnicodeSyntax #-}
module Help.UI.AdminConsole where

import Help.Imports
import Help.Settings

import Control.Lens
import System.Remote.Monitoring
import qualified Data.Text.Encoding as E

adminConsole ∷ Settings → IO ()
adminConsole s = void $ forkServer (E.encodeUtf8 $ adminHost ^$ s) (adminPort ^$ s)
