{-# LANGUAGE NoImplicitPrelude, UnicodeSyntax, OverloadedStrings, TemplateHaskell #-}
module Help.Settings ( -- *The Settings type
                       Settings
                       -- *Lens getters for Settings
                     , ymlFile
                     , logFile
                     , adminHost
                     , adminPort
                     , mongoHost
                       -- *Utilities
                     , loadSettings
                     ) where

import Help.Imports

import Control.Lens.Getter (to, Getter)
import qualified Data.Yaml.Config as YAML (lookup, load)
import Options

data Settings = Settings { _ymlFile  ∷ FilePath
                         , _logFile  ∷ FilePath
                         , _adminHost ∷ FilePath
                         , _adminPort ∷ Int
                         , _mongoHost ∷ String
                         } deriving (Show)

ymlFile ∷ Getter Settings FilePath
ymlFile = to _ymlFile

logFile ∷ Getter Settings FilePath
logFile = to _logFile

adminHost ∷ Getter Settings FilePath
adminHost = to _adminHost

adminPort ∷ Getter Settings Int
adminPort = to _adminPort

mongoHost ∷ Getter Settings String
mongoHost = to _mongoHost

data TempSettings = TempSettings { tempYmlFile  ∷ Maybe FilePath
                                 , tempLogFile  ∷ Maybe FilePath
                                 , tempAdminHost ∷ Maybe String
                                 , tempAdminPort ∷ Maybe Int
                                 , tempMongoHost ∷ Maybe String
                                 }

defineOptions "MainOptions" $ do
    stringOption "cliYml" "yamlFile" "help.yaml"
        "The default YAML configuration file"
    stringOption "cliLogFile" "logFile" ""
        "A log file from which to import"

-- |Loads all settings and creates one authoritative set
loadSettings ∷ IO Settings
loadSettings = do
    cliSettings ← loadCLISettings
    ymlSettings ← loadYMLSettings $ loadableYmlFile $ cliSettings `overrides` defaultSettings
    return $ verifySettings $ cliSettings `overrides` ymlSettings `overrides` defaultSettings

-- |Changes a TempSettings to a Settings, assuming it contains all required options
verifySettings ∷ TempSettings → Settings
verifySettings (TempSettings (Just s1) (Just s2) (Just s3) (Just s4) (Just s5)) = Settings s1 s2 s3 s4 s5
verifySettings _ = error "Not all settings set" -- TODO: Handle this more nicely

-- |Loads command-line settings from the output of @getArgs@
loadCLISettings ∷ IO TempSettings
loadCLISettings = runCommand $ \opts _ ->
                      return $ emptySettings { tempYmlFile = (Just $ cliYml opts), tempLogFile = (Just $ cliLogFile opts) }


-- |Loads settings from a YAML file
loadYMLSettings ∷ FilePath → IO TempSettings
loadYMLSettings yamlFile = do
    yaml <- YAML.load yamlFile
    let aHost = YAML.lookup yaml "adminHost"
        aPort = YAML.lookup yaml "adminPort"
        mHost = YAML.lookup yaml "mongoHost"
    return $ TempSettings Nothing Nothing aHost aPort mHost

-- |Default settings
defaultSettings ∷ TempSettings
defaultSettings = TempSettings (Just "help.yaml") Nothing (Just "localhost") (Just 8080) (Just "127.0.0.1")

-- |Empty settings
emptySettings ∷ TempSettings
emptySettings = TempSettings Nothing Nothing Nothing Nothing Nothing

-- |Takes two TempSettings and returns a new TempSettings, favoring settings in the first over the second
overrides ∷ TempSettings → TempSettings → TempSettings
overrides ns os = let yF = if isJust $ tempYmlFile ns
                             then tempYmlFile ns
                             else tempYmlFile os
                      lF = if isJust $ tempLogFile ns
                             then tempLogFile ns
                             else tempLogFile os
                      aH = if isJust $ tempAdminHost ns
                             then tempAdminHost ns
                             else tempAdminHost os
                      aP = if isJust $ tempAdminPort ns
                             then tempAdminPort ns
                             else tempAdminPort os
                      mH = if isJust $ tempMongoHost ns
                             then tempMongoHost ns
                             else tempMongoHost os
                  in TempSettings yF lF aH aP mH

-- |Pull the YAML file from TempSettings. In the event that the current stack doesn't have a file, fallback to default.
--
-- IN THE EVENT THAT NO DEFAULT FILE EXISTS, THIS WILL LOOP INFINITELY!
loadableYmlFile ∷ TempSettings → FilePath
loadableYmlFile s = case tempYmlFile s of
                      (Just f) → f
                      Nothing  → loadableYmlFile defaultSettings
