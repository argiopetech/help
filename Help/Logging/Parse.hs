{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax       #-}
module Help.Logging.Parse where

import Help.Imports hiding (take, takeWhile)

import Data.Attoparsec.Text
import Data.Attoparsec.Combinator
import Data.Char            (isSpace)

import Database.MongoDB

type Name = Text

data ParseLang = ParseFixed Int Name
               | ParseTill Char Name
               | ParseRest Name
               | FixedString Text

makeRecordParser ∷ Text → Parser Document
makeRecordParser t = if not $ null t
                       then recipeToParser $ lexParser t
                       else error "Parser specification is empty string..."

lexParser ∷ Text → [ParseLang]
lexParser t = case (parseOnly parser t) of
                Right a -> a
                Left  s -> error $ "Failed to parse \'" ++ unpack s ++ "\'"
    where parser = many1' $ fixedLengthParser <|> parseTillParser <|> parseRestParser <|> fixedStringParser
          fixedLengthParser = ParseFixed <$> ((string "%s" *> decimal) <?> "ParseFixed: arg 1")
                                         <*> ((char '`' *> takeWhile (/= '`') <* char '`') <?> "ParseFixed: arg 2")
          parseTillParser   = ParseTill <$> ((string "%s'" *> anyChar <* char '\'') <?> "ParseTill: arg 1")
                                        <*> ((char '`' *> takeWhile (/= '`') <* char '`') <?> "ParseTill: arg 2")
          parseRestParser   = ParseRest <$> (string "%s_" *> char '`' *> takeWhile (/= '`') <* char '`') <?> "ParseRest"
          fixedStringParser = FixedString <$> takeWhile1 (/= '%') <?> "FixedString"

recipeToParser ∷ [ParseLang] → Parser Document
recipeToParser = sequence . go
    where go [] = []
          go ((ParseFixed n name):(FixedString t):ps) = takeChars name n t : go ps
          go ((ParseTill  c name):(FixedString t):ps) = goTill name c t : go ps
          go ((ParseFixed n name):ps) = takeChars name n "" : go ps
          go ((ParseTill  c name):ps) = goTill name c "" : go ps
          go ((ParseRest    name):_) = goTill name '\n' "\n" : []
          go ((FixedString  text):ps) = undefined

takeChars ∷ Label → Int → Text → Parser Field
takeChars l n f = ((\s → l := String s) <$> (take n <* string f))

goTill ∷ Label → Char → Text → Parser Field
goTill l c f = ((\s → l := String s) <$> (takeTill (≡ c) <* string f))
