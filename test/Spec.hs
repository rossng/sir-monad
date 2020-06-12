{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Hspec
import Control.Exception (evaluate)
import DataParser
import Text.Megaparsec
import Data.Either (Either(..))

main :: IO ()
main = hspec $ do
    describe "DataParser.dataParser" $ do
        it "parses simple input" $ do
            let parsed = parse dataParser "" "1,2,3"
            parsed `shouldBe` (Right (Infections [1,2,3]))
        it "parses with spaces" $ do
            let parsed = parse dataParser "" "1, 2 ,3"
            parsed `shouldBe` (Right (Infections [1,2,3]))
        it "parses with newlines" $ do
            let parsed = parse dataParser "" "1\n,2,3\n"
            parsed `shouldBe` (Right (Infections [1,2,3]))
