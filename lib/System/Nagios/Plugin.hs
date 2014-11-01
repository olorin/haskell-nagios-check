{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

data CheckResult = CheckResult CheckStatus Text
  deriving (Ord, Eq, Show)

data PerfValue = RealValue Double | IntegralValue Int64

data PerfDatum = PerfDatum
    { _label :: String
    , _value :: PerfValue
    , _uom   :: UOM
    , _min   :: PerfValue
    , _max   :: PerfValue
    , _warn  :: PerfValue
    , _crit  :: PerfValue
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
defaultResult = CheckResult Unknown $ T.pack "no check result specified"

worstResult :: [CheckResult] -> CheckResult
worstResult rs = case (reverse . sort) rs of
    [] -> defaultResult
    (x:_) -> x

finish :: NagiosPlugin ()
finish = do
    (rs, pds) <- get
    return ()

exitWithStatus :: (CheckStatus, Text) -> IO a
exitWithStatus (OK, t) = putTxt t >> exitWith ExitSuccess
exitWithStatus (r, t) = putTxt t >> exitWith (ExitFailure $ fromEnum r)

putTxt :: Text -> IO ()
putTxt = (putStrLn . T.unpack)
