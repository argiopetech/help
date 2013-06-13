{-# LANGUAGE OverloadedStrings, NoImplicitPrelude, UnicodeSyntax, LambdaCase #-}
module Help.Logging where

import Help.Imports hiding (take)
import Help.Settings
import Help.Logging.Receive
import Help.Logging.Parse
import Help.Logging.Store

import Control.Lens ((^$))

import Data.Attoparsec hiding (takeTill, skipWhile)
import Data.Attoparsec.ByteString.Char8

import Data.Conduit
import Data.Conduit.Attoparsec
import Data.Conduit.Binary hiding (take, lines)

import Data.Text.Encoding as E

import Database.MongoDB

data LogEntry = LogEntry { time :: ByteString
                         , date :: ByteString
                         , severity :: ByteString
                         , message :: ByteString
                         } deriving (Show)

-- |Spawn listerners on all specified ports to receive, parse, and insert logs into a database
logInterface ∷ Settings → IO ()
logInterface s = undefined

-- |Uses a Conduit to iterate over the provided log file, parse log entries, and insert them into a database
loadFile ∷ Settings -> IO ()
loadFile s = do
    pipe <- runIOE $ connect $ host $ mongoHost ^$ s
    runResourceT $ sourceFile (unpack $ logFile ^$ s) $$ sink pipe
    close pipe

sink :: Pipe -> ConduitM ByteString o (ResourceT IO) ()
sink pipe = go
    where go = do
              await >>= \case
                    Nothing  -> return ()
                    (Just m) -> do
                        leftover m
                        l <- sinkParser logEntry

                        _ <- access pipe UnconfirmedWrites "performance" $
                                    insert_ "one_gb" [ "time" := String (E.decodeUtf8 $ time l)
                                                     , "date" := String (E.decodeUtf8 $ date l)
                                                     , "status" := String (E.decodeUtf8 $ severity l)
                                                     , "message" := String (E.decodeUtf8 $ message l) ]
                        go

logEntry :: Parser LogEntry
logEntry = LogEntry <$> (take 8) <* string ", "
                    <*> take 11 <* string " - "
                    <*> takeTill isSpace <* string " - "
                    <*> takeTill (=='\n') <* char '\n'
