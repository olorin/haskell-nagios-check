{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TupleSections              #-}

module System.Nagios.Plugin.PerfData
(
    UOM(..),
    PerfValue(..),
    PerfDatum(..),
    ToPerfData,
    toPerfData,
    barePerfDatum
) where

import           Control.Applicative

import           Data.Int
import           Data.Text       (Text)

import           Numeric

import           Test.QuickCheck

-- | A Nagios "unit of measure". 'NoUOM' translates to an empty
-- string in the check output; it is idiomatic to use it liberally
-- whenever the standard units do not fit.
data UOM =
    Second
  | Millisecond
  | Microsecond
  | Percent
  | Byte
  | Kilobyte
  | Megabyte
  | Gigabyte
  | Terabyte
  | Counter
  | NullUnit
  deriving (Eq)

instance Show UOM where
    show Second      = "s"
    show Millisecond = "ms"
    show Microsecond = "us"
    show Percent     = "%"
    show Byte        = "B"
    show Kilobyte    = "KB"
    show Megabyte    = "MB"
    show Gigabyte    = "GB"
    show Terabyte    = "GB"
    show Counter     = "c"
    show NullUnit    = ""

instance Arbitrary UOM where
    arbitrary = elements $
        [ Second
        , Millisecond
        , Microsecond
        , Percent
        , Byte
        , Kilobyte
        , Megabyte
        , Gigabyte
        , Terabyte
        , Counter
        , NullUnit
        ]

-- | Value of a performance metric.
data PerfValue = RealValue Double | IntegralValue Int64
  deriving (Eq, Ord)

instance Show PerfValue where
    show (RealValue x) = showFFloat Nothing x ""
    show (IntegralValue x) = show x

instance Arbitrary PerfValue where
    arbitrary = oneof $
        [ RealValue <$> arbitrary
        , IntegralValue <$> arbitrary
        ]

-- | One performance metric. A plugin will output zero or more of these,
--   whereupon Nagios generally passes them off to an external system such
--   as <http://oss.oetiker.ch/rrdtool/ RRDTool> or
--   <https://github.com/anchor/vaultaire Vaultaire>.
--   The thresholds are purely informative (designed to be graphed), and
--   do not affect alerting; likewise with `min` and `max`.
data PerfDatum = PerfDatum
    { perfLabel :: Text             -- ^ Name of quantity being measured.
    , perfValue :: PerfValue        -- ^ Measured value, integral or real.
    , perfUom   :: UOM              -- ^ Unit of measure; 'NoUOM' is fine here.
    , perfMin   :: Maybe PerfValue  -- ^ Measured quantity cannot be lower than this.
    , perfMax   :: Maybe PerfValue  -- ^ Measured quantity cannot be higher than this.
    , perfWarn  :: Maybe PerfValue  -- ^ Warning threshold for graphing.
    , perfCrit  :: Maybe PerfValue  -- ^ Critical threshold for graphing.
    }
  deriving (Eq, Show)

class ToPerfData a where
    { toPerfData :: a -> [PerfDatum] }

-- | Create a PerfDatum from only the required values, using Nothing
--   for all the others.
barePerfDatum ::
       Text
    -> PerfValue
    -> UOM
    -> PerfDatum
barePerfDatum info val uom = PerfDatum info val uom Nothing Nothing Nothing Nothing
