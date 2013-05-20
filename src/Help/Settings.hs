{-# LANGUAGE NoImplicitPrelude, UnicodeSyntax #-}
module Help.Settings where

import Help.Imports

import Control.Applicative

data Settings = Settings

loadCLISettings ∷ IO Settings
loadCLISettings = undefined

loadYMLSettings ∷ String → IO Settings
loadYMLSettings yamlFile = undefined

defaultSettings ∷ Settings
defaultSettings = undefined

overrides ∷ Settings → Settings → Settings
overrides ns os = undefined