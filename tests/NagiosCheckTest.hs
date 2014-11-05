{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Hspec

import System.Nagios.Plugin

main :: IO ()
main = hspec suite

suite :: Spec
suite = describe "runNagiosPlugin'" $ it
    "returns the worst result on completion" $ do
        (_,(rs,pds)) <- runNagiosPlugin' universeCheck
        pds `shouldBe` []
        checkInfo (maximum rs) `shouldBe` "universe broken, π < 3"

universeCheck :: NagiosPlugin ()
universeCheck = do
    addResult OK "universe passes basic consistency tests"
    if (pi < (3.0 :: Double)) then addResult Critical "universe broken, π < 3" else return ()
