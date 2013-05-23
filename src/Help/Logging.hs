{-# LANGUAGE NoImplicitPrelude, UnicodeSyntax #-}
module Help.Logging where

import Help.Imports
import Help.Settings
import Help.Logging.Receive
import Help.Logging.Parse
import Help.Logging.Store

-- |Spawn listerners on all specified ports to receive, parse, and insert logs into a database
logInterface ∷ Settings → IO ()
logInterface s = undefined

-- |Uses a Conduit to iterate over the provided log file, parse log entries, and insert them into a database
loadFile ∷ Settings -> IO ()
loadFile f = undefined