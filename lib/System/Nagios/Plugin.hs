module System.Nagios.Plugin
(
    CheckResult(..),
    NagiosPlugin
) where

import Control.Monad.State.Lazy
import Data.Nagios.Perfdata.Metric (UOM)
import Data.Int
import qualified Data.Text as T
import Data.Text (Text)

data CheckStatus = OK | Warning | Critical | Unknown
  deriving (Enum)

instance Show CheckStatus where
    show OK = "OK"
    show Warning = "WARNING"
    show Critical = "CRITICAL"
    show Unknown = "UNKNOWN"

data CheckResult = CheckResult CheckStatus Text

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
newtype CheckState = CheckState ([CheckResult], [PerfDatum])

newtype NagiosPlugin a = NagiosPlugin (StateT CheckState IO a)
