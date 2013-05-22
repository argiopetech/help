{-# LANGUAGE NoImplicitPrelude, UnicodeSyntax #-}
module Help.Settings ( Settings(..)
                     , loadSettings
                     ) where

import Help.Imports

import Control.Applicative

data Settings = Settings { ymlFile ∷ Text }

data TempSettings = TempSettings (Maybe Text)

-- |Loads all settings and creates one authoritative set
loadSettings ∷ IO Settings
loadSettings = do
    args ← getArgs
    let cliSettings = loadCLISettings args
    ymlSettings     ← loadYMLSettings $ tempYmlFile $ cliSettings `overrides` defaultSettings
    return $ verifySettings $ cliSettings `overrides` ymlSettings `overrides` defaultSettings

-- |Changes a TempSettings to a Settings, assuming it contains all required options
verifySettings ∷ TempSettings → Settings
verifySettings = undefined

-- |Loads command-line settings from the output of @getArgs@
loadCLISettings ∷ [Text] → TempSettings
loadCLISettings = undefined

-- |Loads settings from a YAML file
loadYMLSettings ∷ FilePath → IO TempSettings
loadYMLSettings yamlFile = undefined

-- |Default settings
defaultSettings ∷ TempSettings
defaultSettings = undefined

-- |Takes two TempSettings and returns a new TempSettings, favoring settings in the first over the second
overrides ∷ TempSettings → TempSettings → TempSettings
overrides ns os = undefined

-- |Pull the YAML file from TempSettings. In the event that the current stack doesn't have a file, fallback to default.
-- 
-- IN THE EVENT THAT NO DEFAULT FILE EXISTS, THIS WILL LOOP INFINITELY!
tempYmlFile ∷ TempSettings → Text
tempYmlFile (TempSettings (Just t)) = t
tempYmlFile _                       = tempYmlFile defaultSettings