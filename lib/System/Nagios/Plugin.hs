{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module System.Nagios.Plugin
(
    CheckResult(..),
    NagiosPlugin
) where

import Control.Applicative
import Control.Monad
import Control.Monad.State.Lazy
import Data.Nagios.Perfdata.Metric (UOM)
import Data.Int
import Data.List
import qualified Data.Text as T
import Data.Text (Text)
import System.Exit

data CheckStatus = OK | Warning | Critical | Unknown
  deriving (Enum, Eq, Ord)

instance Show CheckStatus where
    show OK = "OK"
    show Warning = "WARNING"
    show Critical = "CRITICAL"
    show Unknown = "UNKNOWN"

type CheckResult = (CheckStatus, Text)

data PerfValue = RealValue Double | IntegralValue Int64

instance Show PerfValue where
    show (RealValue x) = show x
    show (IntegralValue x) = show x

data PerfDatum = PerfDatum
    { _label :: Text
    , _value :: PerfValue
    , _uom   :: UOM
    , _min   :: Maybe PerfValue
    , _max   :: Maybe PerfValue
    , _warn  :: Maybe PerfValue
    , _crit  :: Maybe PerfValue
    }

-- | Current check results/perfdata. If the check suddenly dies, the
--   'worst' of the CheckResults (and all the PerfDatums) will be used
--   to determine the exit state.
type CheckState = ([CheckResult], [PerfDatum])

newtype NagiosPlugin a = NagiosPlugin
  {
    unNagiosPlugin :: StateT CheckState IO a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadState CheckState)

defaultResult :: CheckResult
defaultResult = (Unknown, T.pack "no check result specified")

worstResult :: [CheckResult] -> CheckResult
worstResult rs = case (reverse . sort) rs of
    [] -> defaultResult
    (x:_) -> x

fmtPerfData :: [PerfDatum] -> Text
fmtPerfData = (T.intercalate " ") . map fmtPerfDatum
  where
    fmtPerfDatum PerfDatum{..} = T.concat $
        [ _label
        , "="
        , (T.pack . show) _value
        , (T.pack . show) _uom
        , fmtThreshold _min
        , fmtThreshold _max
        , fmtThreshold _warn
        , fmtThreshold _crit
        ]

    fmtThreshold Nothing = ";"
    fmtThreshold (Just t) = T.pack . concat $ [";", show t]

fmtResults :: [CheckResult] -> Text
fmtResults = fmtResult . worstResult
  where
    fmtResult (s,t) = T.concat $
        [ (T.pack . show) s
        , ": "
        , t
        ]

checkOutput :: NagiosPlugin Text
checkOutput = do
    (rs, pds) <- get
    return . T.concat $
        [ fmtResults rs
        , " | "
        , fmtPerfData pds
        ]

finalStatus :: NagiosPlugin CheckStatus
finalStatus = get >>= (return . fst . worstResult) . fst

finish :: NagiosPlugin ()
finish = do
    s <- finalStatus
    o <- checkOutput
    liftIO $ exitWithStatus (s, o)

exitWithStatus :: (CheckStatus, Text) -> IO a
exitWithStatus (OK, t) = putTxt t >> exitWith ExitSuccess
exitWithStatus (r, t) = putTxt t >> exitWith (ExitFailure $ fromEnum r)

putTxt :: Text -> IO ()
putTxt = (putStrLn . T.unpack)
