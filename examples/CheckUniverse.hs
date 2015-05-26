{-# LANGUAGE OverloadedStrings #-}

import           System.Nagios.Plugin

universeCheck :: Double -> NagiosPlugin ()
universeCheck pi' = do
    addResult OK "universe passes basic consistency tests"
    if (pi' < (3.0 :: Double)) then addResult Critical "universe broken, π < 3" else return ()

main :: IO ()
main = runNagiosPlugin (universeCheck 3.1415)
