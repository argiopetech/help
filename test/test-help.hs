module Main where

import ClassyPrelude
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

unformatPhoneNumber :: String -> String
unformatPhoneNumber = undefined

formatPhoneNumber :: String -> String
formatPhoneNumber = undefined

spec :: Spec
spec = do
  describe "unformatPhoneNumber" $ do

    it "all works" True

    it "removes dashes, spaces, and parenthesies" $
      unformatPhoneNumber "(555) 555-1234" == "5555551234"

    it "handles non-US phone numbers" $
      pendingWith "need to look up how other cultures format phone numbers"

    it "converts letters to numbers" $ do
      let expected = "6862377"
          actual   = unformatPhoneNumber "NUMBERS"
      actual == expected

    it "can add and remove formatting without changing the number" $ property $
      forAll phoneNumber $ \n -> unformatPhoneNumber (formatPhoneNumber n) == n

phoneNumber :: Gen String
phoneNumber = do
  n <- elements [7,10,11,12,13,14,15]
  vectorOf n (elements "0123456789")
