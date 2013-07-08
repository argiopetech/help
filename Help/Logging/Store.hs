{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Help.Logging.Store where

import Help.Imports       hiding (take)
import Help.Logging.Parse

import Control.Monad (void)

import Data.Conduit            (MonadThrow, Sink, await, leftover, (=$))
import Data.Conduit.Attoparsec (sinkParser)
import Data.Conduit.Text       (decode, utf8)

import Data.Attoparsec.Text (Parser)
import Data.Text.Encoding as E
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as S

import Database.MongoDB (Pipe, AccessMode(UnconfirmedWrites), access, insertMany_, Field((:=)), Value(String), Document)

sink ∷ forall (m ∷ * → *). (MonadThrow m, MonadIO m) ⇒ Pipe → Parser Document → Text → Text → Sink ByteString m ()
sink pipe parser db col = decode utf8 =$ go mempty
    where {-# INLINE go #-}
          go ∷ Seq Document -> Sink Text m ()
          go s = do
              await >>= \case
                    Nothing  → doInsert s
                    (Just m) → do
                        leftover m
                        l <- sinkParser parser

                        if S.length s == 50
                          then do
                              doInsert s
                              go mempty
                          else go (s |> l)
          doInsert = void . access pipe UnconfirmedWrites db . insertMany_ col . toList
{-# INLINE sink #-}
