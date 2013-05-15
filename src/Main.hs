module Main where

import Help.Logging
import Help.Settings
import Help.UI

import Control.Concurrent

main :: IO ()
main = do
    s <- loadSettings undefined

    _ <- forkIO $ undefined
    undefined