{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TupleSections              #-}

module System.Nagios.Plugin.Range
(
    Range
) where

import           System.Nagios.Plugin.Check (PerfValue)

-- | A 'Range' is a combination of a lower boundary and an upper boundary (x,y).
--   An 'AcceptableRange' asserts that measured values between x and y
--   imply that nothing is wrong; an UnacceptableRange implies the inverse.
data Range = AcceptableRange PerfValue PerfValue
           | UnacceptableRange PerfValue PerfValue
