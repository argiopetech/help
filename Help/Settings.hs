{-# LANGUAGE NoImplicitPrelude, UnicodeSyntax, OverloadedStrings, TemplateHaskell #-}
module Help.Settings ( -- *The Settings type
                       Settings
                       -- *Lens getters for Settings
                     , ymlFile
                     , logFile
                     , logCollection
                     , logParser
                     , adminHost
                     , adminPort
                     , database
                     , servers
                     , mongoHost
                       -- *The TCPConnection type
                     , TCPConnection
                       -- *Lens getters for TCPConnections
                     , port
                     , collection
                     , parser
                       -- *Utilities
                     , loadSettings
                     ) where

import Help.Imports
import Help.Logging.Parse

import Control.Lens.Getter (to, Getter)
import qualified Data.Yaml.Config as YAML (lookup, load)

import Data.Aeson.Types hiding (Parser, parse)

import Data.Attoparsec.Text
import Database.MongoDB (Document)

import Options

data Settings = Settings { _ymlFile  ∷ FilePath
                         , _logFile  ∷ FilePath
                         , _logCollection ∷ Text
                         , _logParser ∷ Maybe (Parser Document)
                         , _database ∷ Text
                         , _adminHost ∷ FilePath
                         , _adminPort ∷ Int
                         , _servers ∷ [TCPConnection]
                         , _mongoHost ∷ String
                         }



data TCPConnection = TCPConnection { _port ∷ Int
                                   , _collection ∷ Text
                                   , _parser ∷ Parser Document
                                   }

data TempSettings = TempSettings { tempYmlFile  ∷ Maybe FilePath
                                 , tempLogFile  ∷ Maybe FilePath
                                 , tempLogCollection ∷ Maybe Text
                                 , tempLogParser ∷ Maybe (Parser Document)
                                 , tempDatabase ∷ Maybe Text
                                 , tempAdminHost ∷ Maybe String
                                 , tempAdminPort ∷ Maybe Int
                                 , tempServers ∷ Maybe [TCPConnection]
                                 , tempMongoHost ∷ Maybe String
                                 }

instance FromJSON TCPConnection where
    parseJSON (Object v) = do
        testRecord <- v .: "sampleRecord"
        parserLine <- v .: "recordFormat"
        let p = makeRecordParser parserLine
        if not $ goodParser p testRecord
           then error $ "Specified parser does not parse line " ++ unpack parserLine
           else do
               TCPConnection <$> v .: "port"
                             <*> v .: "collection"
                             <*> (pure $! p)
    parseJSON _        = mempty

goodParser ∷ Parser Document → Text → Bool
goodParser p t = let parseResult = parse p t
                 in case parseResult of
                      Done "" _ -> True
                      _         -> False

ymlFile ∷ Getter Settings FilePath
ymlFile = to _ymlFile

logFile ∷ Getter Settings FilePath
logFile = to _logFile

logCollection ∷ Getter Settings Text
logCollection = to _logCollection

logParser ∷ Getter Settings (Maybe (Parser Document))
logParser = to _logParser

database ∷ Getter Settings Text
database = to _database

adminHost ∷ Getter Settings FilePath
adminHost = to _adminHost

adminPort ∷ Getter Settings Int
adminPort = to _adminPort

servers ∷ Getter Settings [TCPConnection]
servers = to _servers

port ∷ Getter TCPConnection Int
port = to _port

collection ∷ Getter TCPConnection Text
collection = to _collection

parser ∷ Getter TCPConnection (Parser Document)
parser = to _parser

mongoHost ∷ Getter Settings String
mongoHost = to _mongoHost

defineOptions "MainOptions" $ do
    stringOption "cliYml" "yamlFile" "help.yaml"
        "The default YAML configuration file"
    stringOption "cliLogFile" "logFile" ""
        "A log file from which to import"
    textOption "cliLogCollection" "collection" "default"
        "The MongoDB collection to import to"
    textOption "cliRecordFormat" "recordFormat" ""
        "The format of the records in the log file"

-- |Loads all settings and creates one authoritative set
loadSettings ∷ IO Settings
loadSettings = do
    cliSettings ← loadCLISettings
    ymlSettings ← loadYMLSettings $ loadableYmlFile $ cliSettings `overrides` defaultSettings
    return $ verifySettings $ cliSettings `overrides` ymlSettings `overrides` defaultSettings

-- |Changes a TempSettings to a Settings, assuming it contains all required options
verifySettings ∷ TempSettings → Settings
verifySettings (TempSettings (Just s1) (Just s2) (Just s3) s4 (Just s5) (Just s6) (Just s7) (Just s8) (Just s9)) = Settings s1 s2 s3 s4 s5 s6 s7 s8 s9
verifySettings _ = error "Not all settings set" -- TODO: Handle this better

-- |Loads command-line settings from the output of @getArgs@
loadCLISettings ∷ IO TempSettings
loadCLISettings = runCommand $ \opts _ -> do
                      let p = if null $ cliYml opts
                                then Nothing
                                else Just $! makeRecordParser $ cliRecordFormat opts
                      return $ emptySettings { tempYmlFile = (Just $ cliYml opts)
                                             , tempLogFile = (Just $ cliLogFile opts)
                                             , tempLogCollection = (Just $ cliLogCollection opts)
                                             , tempLogParser = p
                                             }


-- |Loads settings from a YAML file
loadYMLSettings ∷ FilePath → IO TempSettings
loadYMLSettings yamlFile = do
    yaml <- YAML.load yamlFile
    let aHost = YAML.lookup yaml "adminHost"
        aPort = YAML.lookup yaml "adminPort"
        serve = YAML.lookup yaml "servers"
        mHost = YAML.lookup yaml "mongoHost"
        datab = YAML.lookup yaml "database"
    return $ TempSettings Nothing Nothing datab Nothing Nothing aHost aPort serve mHost

-- |Default settings
defaultSettings ∷ TempSettings
defaultSettings = TempSettings (Just "help.yaml") Nothing Nothing Nothing (Just "help-db") (Just "localhost") (Just 8080) Nothing (Just "127.0.0.1")

-- |Empty settings
emptySettings ∷ TempSettings
emptySettings = TempSettings Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- |Takes two TempSettings and returns a new TempSettings, favoring settings in the first over the second
overrides ∷ TempSettings → TempSettings → TempSettings
overrides ns os = let yF = if isJust $ tempYmlFile ns
                             then tempYmlFile ns
                             else tempYmlFile os
                      lF = if isJust $ tempLogFile ns
                             then tempLogFile ns
                             else tempLogFile os
                      lC = if isJust $ tempLogCollection ns
                             then tempLogCollection ns
                             else tempLogCollection os
                      lP = if isJust $ tempLogParser ns
                             then tempLogParser ns
                             else tempLogParser os
                      db = if isJust $ tempDatabase ns
                             then tempDatabase ns
                             else tempDatabase os
                      aH = if isJust $ tempAdminHost ns
                             then tempAdminHost ns
                             else tempAdminHost os
                      aP = if isJust $ tempAdminPort ns
                             then tempAdminPort ns
                             else tempAdminPort os
                      ss = if isJust $ tempServers ns
                             then tempServers ns
                             else tempServers os
                      mH = if isJust $ tempMongoHost ns
                             then tempMongoHost ns
                             else tempMongoHost os
                  in TempSettings yF lF lC lP db aH aP ss mH

-- |Pull the YAML file from TempSettings. In the event that the current stack doesn't have a file, fallback to default.
--
-- IN THE EVENT THAT NO DEFAULT FILE EXISTS, THIS WILL LOOP INFINITELY!
loadableYmlFile ∷ TempSettings → FilePath
loadableYmlFile s = case tempYmlFile s of
                      (Just f) → f
                      Nothing  → loadableYmlFile defaultSettings
