module Main where

import ClassyPrelude
import Test.Hspec
import Test.QuickCheck
import Logging_Parse

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Logging" $ do
           parserGeneratorSpec
