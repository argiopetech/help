{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Help.Logging.Store where

import Help.Imports       hiding (take)
import Help.Logging.Parse

import Data.Conduit            (MonadThrow, Sink, await, leftover, (=$))
import Data.Conduit.Attoparsec (sinkParser)
import Data.Conduit.Text       (decode, utf8)

import Data.Attoparsec.Text (Parser)
import Data.Text.Encoding as E

import Database.MongoDB (Pipe, AccessMode(UnconfirmedWrites), access, insert_, Field((:=)), Value(String), Document)

sink ∷ forall (m ∷ * → *). (MonadThrow m, MonadIO m) ⇒ Pipe → Parser Document → Text → Text → Sink ByteString m ()
sink pipe parser db col = decode utf8 =$ go
    where go ∷ Sink Text m ()
          go = do
              await >>= \case
                    Nothing  → return ()
                    (Just m) → do
                        leftover m
                        l <- sinkParser parser

                        _ <- access pipe UnconfirmedWrites db $
                                    insert_ col l
                        go
