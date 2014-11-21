{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TupleSections              #-}

module System.Nagios.Plugin.Check
(
    CheckStatus(..),
    CheckResult,
    UOM(..),
    PerfValue(..),
    NagiosPlugin,
    PerfDatum
) where

import           Control.Applicative
import           Control.Monad.Catch
import           Control.Monad.State.Lazy
import           Data.Bifunctor
import           Data.Int
import           Data.Monoid
import           Data.Nagios.Perfdata.Metric (UOM (..))
import           Data.Text                   (Text)
import qualified Data.Text                   as T

-- | Nagios plugin exit statuses. Ordered by priority -
--   OK < Warning < Critical < Unknown, which correspond to plugin exit
--   statuses of 0, 1, 2, and 3 respectively.
data CheckStatus = OK       -- ^ Check executed successfully and
                            --   detected no service problems.
                 | Warning  -- ^ Nothing's actually broken but this
                            --   should be followed up.
                 | Critical -- ^ Check executed successfully and detected
                            --   a service failure.
                 | Unknown  -- ^ Check unable to determine service
                            --   status.
  deriving (Enum, Eq, Ord)

instance Show CheckStatus where
    show OK = "OK"
    show Warning = "WARNING"
    show Critical = "CRITICAL"
    show Unknown = "UNKNOWN"

-- | A CheckResult is the exit status of the plugin combined with the
--   plugin's info text. A `NagiosPlugin` which exits with
--
--   > CheckResult (Critical "entropy decreasing in closed system")
--
--   as its peak-badness CheckResult (and no 'PerfDatum's) will a) exit with
--   status 2 and b) output the text "CRITICAL: entropy decreasing in closed
--   system".
newtype CheckResult = CheckResult
  { unCheckResult :: (CheckStatus, Text) }
    deriving (Eq, Ord, Show)

-- | Value of a performance metric.
data PerfValue = RealValue Double | IntegralValue Int64
  deriving (Eq, Ord, Num)

instance Show PerfValue where
    show (RealValue x) = show x
    show (IntegralValue x) = show x

-- | One performance metric. A plugin will output zero or more of these,
--   whereupon Nagios generally passes them off to an external system such
--   as <http://oss.oetiker.ch/rrdtool/ RRDTool> or
--   <https://github.com/anchor/vaultaire Vaultaire>.
--   The thresholds are purely informative (designed to be graphed), and
--   do not affect alerting; likewise with `_min` and `_max`.
data PerfDatum = PerfDatum
    { _label :: Text             -- ^ Name of quantity being measured.
    , _value :: PerfValue        -- ^ Measured value, integral or real.
    , _uom   :: UOM              -- ^ Unit of measure; 'NullUOM' is fine here.
    , _min   :: Maybe PerfValue  -- ^ Measured quantity cannot be lower than this.
    , _max   :: Maybe PerfValue  -- ^ Measured quantity cannot be higher than this.
    , _warn  :: Maybe PerfValue  -- ^ Warning threshold for graphing.
    , _crit  :: Maybe PerfValue  -- ^ Critical threshold for graphing.
    }
  deriving (Eq, Show)

-- | Current check results/perfdata. If the check suddenly dies, the
--   'worst' of the CheckResults (and all the PerfDatums) will be used
--   to determine the exit state.
type CheckState = ([CheckResult], [PerfDatum])

newtype NagiosPlugin a = NagiosPlugin
  {
    unNagiosPlugin :: StateT CheckState IO a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadState CheckState, MonadCatch, MonadThrow)

-- | A 'Range' is a combination of a lower boundary and an upper boundary (x,y).
--   An 'AcceptableRange' asserts that measured values between x and y
--   imply that nothing is wrong; an UnacceptableRange implies the inverse.
data Range = AcceptableRange PerfValue PerfValue
           | UnacceptableRange PerfValue PerfValue
