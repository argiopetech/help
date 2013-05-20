{-# LANGUAGE NoImplicitPrelude, UnicodeSyntax #-}
module Main where

import Help.Imports
import Help.Logging
import Help.Settings
import Help.UI.WebSearch
import Help.UI.AdminConsole

import Control.Concurrent

main ∷ IO ()
main = do
    cliSettings ← loadCLISettings
    ymlSettings ← loadYMLSettings yamlFile
    let settings = cliSettings `overrides` ymlSettings `overrides` defaultSettings


    _ ← forkIO $ webSearch settings
    _ ← forkIO $ adminConsole settings
    undefined

yamlFile ∷ String
yamlFile = undefined