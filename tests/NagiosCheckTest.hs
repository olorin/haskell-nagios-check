{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text)
import Test.Hspec

import System.Nagios.Plugin

main :: IO ()
main = hspec suite

suite :: Spec
suite = describe "runNagiosPlugin'" $ it
    "returns explicitly added result in favor of default" $ do
        (_,(rs,pds)) <- runNagiosPlugin' (universeCheck pi)
        pds `shouldBe` []
        checkInfo (maximum rs) `shouldBe` universeGoodResult

universeCheck :: Double -> NagiosPlugin ()
universeCheck pi' = do
    addResult OK universeGoodResult
    if (pi' < (3.0 :: Double)) then addResult Critical universeBadResult else return ()

universeGoodResult :: Text
universeGoodResult = "universe passes basic consistency tests"

universeBadResult :: Text
universeBadResult = "universe broken, π < 3"
