{-# LANGUAGE NoImplicitPrelude, UnicodeSyntax #-}
module Main (main) where

import Help.Imports
import Help.Logging
import Help.Settings
import Help.UI.WebSearch
import Help.UI.AdminConsole

import Control.Concurrent hiding (forkIO)
import Control.Concurrent.STM
import Control.Concurrent.Thread.Group

import Control.Lens ((^.))

main ∷ IO ()
main = do
    settings ← loadSettings

    if settings ^. loadMode
      then loadFile settings
      else do
          g  ← new -- Creates a new thread group
          (t1, _) ← forkIO g $ webSearch settings
          (t2, _) ← forkIO g $ adminConsole settings
          (t3, _) ← forkIO g $ logInterface settings

          -- Cleans up all threads if any thread dies
          -- Each thread will need to clean up internally
          waitAny g `finally` mapM_ killThread [t1, t2, t3]


-- |Unlike @wait@ (which waits for all threads to exit), @waitAny@ waits for any thread to exit, then returns
waitAny ∷ ThreadGroup → IO ()
waitAny tg = do
    on ← atomically $ nrOfRunning tg
    atomically $ nrOfRunning tg >>= \n → when (n ≡ on) retry