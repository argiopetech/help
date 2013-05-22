{-# LANGUAGE NoImplicitPrelude, UnicodeSyntax #-}
module Help.Logging where

import Help.Imports
import Help.Settings
import Help.Logging.Receive
import Help.Logging.Parse
import Help.Logging.Store

logInterface ∷ Settings -> IO ()
logInterface s = undefined