{-# LANGUAGE OverloadedStrings, NoImplicitPrelude, UnicodeSyntax, LambdaCase, ExplicitForAll, KindSignatures, TemplateHaskell, FlexibleContexts #-}
module Help.Logging.Receive where

import Help.Imports hiding (take)
import Help.Settings
import Help.Logging.Store (sink)

import Control.Monad.Trans.Resource
import Data.Conduit
import Data.Conduit.Binary hiding (take, lines)
import Data.Conduit.Network

import Data.Attoparsec.Text (Parser)
import Database.MongoDB

import Control.Lens.Getter ((^.))

-- |Spawn listerners on all specified ports to receive, parse, and insert logs into a database
logInterface ∷ (MonadResource m, MonadBaseControl IO m) => Settings → m ()
logInterface s = do
    forM_ (s^.servers) $
                 \c -> runTCPServer (serverSettings (c^.port) HostAny) (app (c^.collection) (c^.parser))
        where app ∷ MonadResource m => Text → Parser Document → Application m
              app c p a = do
                     (rk, pipe) ← allocate (runIOE $ connect $ host $ s^.mongoHost) close
                     appSource a $$ sink pipe p (s^.database) c
                     release rk

-- |Uses a Conduit to iterate over the provided log file, parse log entries, and insert them into a database
loadFile ∷ MonadResource m => Settings -> m ()
loadFile s = do
    (rk, pipe) ← allocate (runIOE $ connect $ host $ s^.mongoHost) close
    sourceFile (unpack $ s^.logFile) $$ sink pipe mempty (s^.database) (s^.logCollection)
    release rk
