{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import ClassyPrelude
import Criterion.Main

import Help.Logging.Parse

import Data.Attoparsec.Text

import Database.MongoDB.Internal.Protocol
import Database.MongoDB

import System.IO

main :: IO ()
main = do
    -- For me, dd if=/dev/zero of=/dev/null runs at about 1.3GB/s, so /dev/null should be fast enough.
    devnull <- openFile "/dev/null" ReadWriteMode
    pipe <- newPipe devnull

    defaultMain [ bench_takeChars
                , bench_goTill
                , bench_realParsers
                , bench_mongoSerialize pipe
                ]

    close pipe

-- Experiment design:
-- My parsers
--  * takeChars
--  ** Long string, short string
--     Should be linear relationship
bench_takeChars :: Benchmark
bench_takeChars = bgroup "takeChars" [ bench_takeCharsShortString, bench_takeCharsLongString, bench_takeCharsVeryLongString ]

bench_takeCharsShortString :: Benchmark
bench_takeCharsShortString = let parser = takeChars "fakeLabel" 1 ""
                                 text = pack $ replicate 1000 'a'
                             in bench "1 character" $ whnf (parse parser) text

bench_takeCharsLongString :: Benchmark
bench_takeCharsLongString = let parser = takeChars "fakeLabel" 1000 ""
                                text = pack $ replicate 1000 'a'
                            in bench "1000 characters" $ whnf (parse parser) text

bench_takeCharsVeryLongString :: Benchmark
bench_takeCharsVeryLongString = let parser = takeChars "fakeLabel" 10000 ""
                                    text = pack $ replicate 10000 'a'
                                in bench "10000 characters" $ whnf (parse parser) text

--  * goTill
--  ** Long string, short string
--     Should be linear relationship with length

bench_goTill :: Benchmark
bench_goTill = bgroup "goTill" [ bench_goTillShortString, bench_goTillLongString, bench_goTillVeryLongString ]

bench_goTillShortString :: Benchmark
bench_goTillShortString =let parser = goTill "fakeLabel" '_' ""
                             text = pack $ "a_"
                         in bench "1 character" $ whnf (parse parser) text

bench_goTillLongString :: Benchmark
bench_goTillLongString = let parser = goTill "fakeLabel" '_' ""
                             text = pack $ replicate 1000 'a' ++ "_"
                         in bench "1000 characters" $ whnf (parse parser) text

bench_goTillVeryLongString :: Benchmark
bench_goTillVeryLongString = let parser = goTill "fakeLabel" '_' ""
                                 text = pack $ replicate 10000 'a' ++ "_"
                             in bench "10000 characters" $ whnf (parse parser) text


--  * Real parsers (combination)
--  ** Multi-record, single record with same string length
singleLog :: Text
singleLog = "00:00:01, 01/Jan/1970 - Error - xbmspts owtt cube zndm iegwlqq nnlqdr oqnn qurq bpypdx\n"

bench_realParsers :: Benchmark
bench_realParsers = bgroup "Log parsing" [ bench_singleRecord, bench_multiRecord ]

bench_singleRecord :: Benchmark
bench_singleRecord = let parser = makeRecordParser $ "%s_`message`"
                     in bench "Single-record" $ whnf (parse parser) singleLog

bench_multiRecord :: Benchmark
bench_multiRecord = let parser = makeRecordParser $ "%s8`time`, %s11`date` - %s' '`status` - %s_`message`"
                     in bench "Multi-record" $ whnf (parse parser) singleLog

-- Insertion process
--  * Single document
bench_mongoSerialize :: Pipe -> Benchmark
bench_mongoSerialize pipe = bgroup "MongoDB Serialization" [ bench_singleDocument pipe, bench_multiDocument pipe ]

bench_singleDocument :: Pipe -> Benchmark
bench_singleDocument pipe = bgroup "1 record" [ bench_mongoSingleShort pipe, bench_mongoSingleLong pipe, bench_mongoSingleVeryLong pipe, bench_mongoSingleOneDocument pipe ]

bench_mongoSingleShort :: Pipe -> Benchmark
bench_mongoSingleShort pipe = do
    let record = ["cats" := String "a" ]:[]
        db = "help"
        col = "benchmark"
    bench "1 character" $ whnfIO $ (access pipe UnconfirmedWrites db $ insertMany_ col record) >>= (\(Right _) -> return ())

bench_mongoSingleLong :: Pipe -> Benchmark
bench_mongoSingleLong pipe = do
    let record = ["cats" := String (pack $ replicate 1000 'a') ]:[]
        db = "help"
        col = "benchmark"
    bench "1000 characters" $ whnfIO $ (access pipe UnconfirmedWrites db $ insertMany_ col record) >>= (\(Right _) -> return ())

bench_mongoSingleVeryLong :: Pipe -> Benchmark
bench_mongoSingleVeryLong pipe = do
    let record = ["cats" := String (pack $ replicate 10000 'a') ]:[]
        db = "help"
        col = "benchmark"
    bench "10000 character" $ whnfIO $ (access pipe UnconfirmedWrites db $ insertMany_ col record) >>= (\(Right _) -> return ())

bench_mongoSingleOneDocument :: Pipe -> Benchmark
bench_mongoSingleOneDocument pipe = do
    let db = "help"
        col = "benchmark"
        parser = makeRecordParser $ "%s_`message`"
        (Right doc) = parseOnly parser singleLog
        insertable = doc:[]
    bench "1 document" $ whnfIO $ (access pipe UnconfirmedWrites db $ insertMany_ col insertable) >>= (\(Right _) -> return ())


--  * Multi-record
bench_multiDocument :: Pipe -> Benchmark
bench_multiDocument pipe = bgroup "4 record" [ bench_mongoMultiOneDocument pipe ] -- , bench_mongoMulti50Document pipe, bench_mongoMulti100Document pipe ]

bench_mongoMultiOneDocument :: Pipe -> Benchmark
bench_mongoMultiOneDocument pipe = do
    let db = "help"
        col = "benchmark"
        parser = makeRecordParser $ "%s8`time`, %s11`date` - %s' '`status` - %s_`message`"
        (Right doc) = parseOnly parser singleLog
        insertable = doc:[]
    bench "1 document" $ whnfIO $ (access pipe UnconfirmedWrites db $ insertMany_ col insertable) >>= (\(Right _) -> return ())

bench_mongoMulti50Document :: Pipe -> Benchmark
bench_mongoMulti50Document pipe = do
    let db = "help"
        col = "benchmark"
        parser = makeRecordParser $ "%s8`time`, %s11`date` - %s' '`status` - %s_`message`"
        (Right doc) = parseOnly parser singleLog
        insertable = replicate 50 doc
    bench "50 document" $ whnfIO $ (access pipe UnconfirmedWrites db $ insertMany_ col insertable) >>= (\(Right _) -> return ())

bench_mongoMulti100Document :: Pipe -> Benchmark
bench_mongoMulti100Document pipe = do
    let db = "help"
        col = "benchmark"
        parser = makeRecordParser $ "%s8`time`, %s11`date` - %s' '`status` - %s_`message`"
        (Right doc) = parseOnly parser singleLog
        insertable = replicate 100 doc
    bench "500 document" $ whnfIO $ (access pipe UnconfirmedWrites db $ insertMany_ col insertable) >>= (\(Right _) -> return ())
