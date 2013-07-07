{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE StandaloneDeriving  #-}
module Logging_Parse where

import ClassyPrelude
import Test.Hspec
import Test.QuickCheck

import Help.Logging.Parse

import Data.Attoparsec.Text

import Database.MongoDB

deriving instance Show ParseLang
deriving instance Eq ParseLang

singleLog ∷ Text
singleLog = "00:00:01, 01/Jan/1970 - Error - xbmspts owtt cube zndm iegwlqq nnlqdr oqnn qurq bpypdx\n"

parseRecipe ∷ Text
parseRecipe = "%s8`time`, %s11`date` - %s' '`status` - %s_`message`"

lexedRecipe ∷ [ParseLang]
lexedRecipe = [ParseFixed 8 "time", FixedString ", ", ParseFixed 11 "date", FixedString " - ", ParseTill ' ' "status", FixedString " - ", ParseRest "message"]

handLogEntry ∷ Parser Document
handLogEntry = sequence $ takeChars "time" 8 ", "
                    : takeChars "date" 11 " - "
                    : goTill "status" ' ' " - "
                    : goTill "message" '\n' "\n"
                    : []

parserGeneratorSpec :: Spec
parserGeneratorSpec = do
  describe "Parser Generator" $ do
    it "Converts a recipe to its lexed representation" $ do
        lexParser parseRecipe `shouldBe` lexedRecipe
    it "Converts a lexed recipe into a correct parser" $ do
        let res1 = maybeResult $ parse handLogEntry singleLog
            res2 = maybeResult $ parse (recipeToParser lexedRecipe) singleLog
        res1 `shouldBe` res2
    it "Whole chain works" $ do
        let res1 = maybeResult $ parse (recipeToParser $ lexParser $ parseRecipe) singleLog
            res2 = maybeResult $ parse (recipeToParser lexedRecipe) singleLog
        res1 `shouldBe` res2
