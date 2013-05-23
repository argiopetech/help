{-# LANGUAGE NoImplicitPrelude, UnicodeSyntax #-}
module Help.Settings ( -- *The Settings type
                       Settings
                       -- *Lens getters for Settings
                     , ymlFile
                     , loadMode
                     , logFile
                       -- *Utilities
                     , loadSettings
                     ) where

import Help.Imports

import Control.Applicative
import Control.Lens.Getter (to, Getter)

data Settings = Settings { _ymlFile  ∷ FilePath
                         , _loadMode ∷ Bool
                         , _logFile  ∷ FilePath
                         } deriving (Show)

ymlFile ∷ Getter Settings Text
ymlFile = to _ymlFile

loadMode ∷ Getter Settings Bool
loadMode = to _loadMode

logFile ∷ Getter Settings Text
logFile = to _logFile

data TempSettings = TempSettings { tempYmlFile  ∷ (Maybe FilePath)
                                 , tempLoadMode ∷ (Maybe Bool)
                                 , tempLogFile  ∷ (Maybe FilePath)
                                 }

-- |Loads all settings and creates one authoritative set
loadSettings ∷ IO Settings
loadSettings = do
    args ← getArgs
    let cliSettings = loadCLISettings args
    ymlSettings     ← loadYMLSettings $ loadableYmlFile $ cliSettings `overrides` defaultSettings
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
loadableYmlFile ∷ TempSettings → Text
loadableYmlFile s = case tempYmlFile s of
                      (Just f) → f
                      Nothing  → loadableYmlFile defaultSettings