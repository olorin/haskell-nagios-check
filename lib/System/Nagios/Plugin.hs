{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module System.Nagios.Plugin
(
    CheckStatus(..),
    CheckResult,
    NagiosPlugin,
    runNagiosPlugin,
    addPerfDatum
) where

import           Control.Applicative
import           Control.Exception           (Exception, SomeException)
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.State.Lazy
import           Data.Int
import           Data.List
import           Data.Nagios.Perfdata.Metric (UOM)
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           System.Exit

-- | Nagios plugin exit statuses. Ordered by priority -
--   OK < Warning < Critical < Unknown, which correspond to plugin exit
--   statuses of 0, 1, 2, and 3 respectively.
data CheckStatus = OK       -- | Check executed successfully and
                            --   detected no service problems.
                 | Warning  -- | Nothing's actually broken but this
                            --   should be followed up.
                 | Critical -- | Check executed successfully and detected
                            --   a service failure.
                 | Unknown  -- | Check unable to determine service
                            --   status.
  deriving (Enum, Eq, Ord)

instance Show CheckStatus where
    show OK = "OK"
    show Warning = "WARNING"
    show Critical = "CRITICAL"
    show Unknown = "UNKNOWN"

newtype CheckResult = CheckResult
  { unCheckResult :: (CheckStatus, Text) }
    deriving (Eq, Ord, Show)

checkStatus :: CheckResult -> CheckStatus
checkStatus (CheckResult (s,_)) = s

checkInfo :: CheckResult -> Text
checkInfo (CheckResult (_,msg)) = msg

-- | Value of a performance metric.
data PerfValue = RealValue Double | IntegralValue Int64

instance Show PerfValue where
    show (RealValue x) = show x
    show (IntegralValue x) = show x

-- | One performance metric. A plugin will output zero or more of these.
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

runNagiosPlugin :: NagiosPlugin a -> IO ()
runNagiosPlugin (NagiosPlugin a) = evalStateT (a >> finish) ([],[]) >> return ()

-- | Insert a result. Only the 'CheckStatus' with the most 'badness'
--   will determine the check's exit status.
addResult :: CheckStatus -> Text -> NagiosPlugin ()
addResult s t = do
    (rs, pds) <- get
    put ((CheckResult (s, t)) : rs, pds)

-- | Insert a performance metric into the list the check will output.
addPerfDatum ::
    Text ->            -- ^ Name of the quantity being measured.
    PerfValue ->       -- ^ Measured value.
    UOM ->             -- ^ Unit of the measured value.
    Maybe PerfValue -> -- ^ Minimum threshold.
    Maybe PerfValue -> -- ^ Maximum threshold.
    Maybe PerfValue -> -- ^ Warning threshold.
    Maybe PerfValue -> -- ^ Critical threshold.
    NagiosPlugin ()
addPerfDatum info val uom min max warn crit =
    addPerfDatum' $ PerfDatum info val uom min max warn crit
  where
    addPerfDatum' pd = do
        (rs, pds) <- get
        put (rs, pd : pds)

defaultResult :: CheckResult
defaultResult = CheckResult (Unknown, T.pack "no check result specified")

panicResult :: Exception e => e -> (CheckStatus, Text)
panicResult e = (Critical,
                T.pack ("unhandled exception: " ++ show e))

-- | Returns result with greatest badness, or a default UNKNOWN result
--   if no results have been specified.
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
    fmtResult (CheckResult (s,t)) = T.concat $
        [ (T.pack . show) s
        , ": "
        , t
        ]

checkOutput :: CheckState -> Text
checkOutput (rs, pds) = T.concat $
        [ fmtResults rs
        , " | "
        , fmtPerfData pds
        ]

finalStatus :: CheckState -> CheckStatus
finalStatus = (checkStatus . worstResult) . fst

-- | Calculate our final result, print output and then exit with the
--   appropriate status.
finish :: StateT CheckState IO ()
finish = do
    st <- get
    liftIO $ exitWithStatus (finalStatus st, checkOutput st)

exitWithStatus :: (CheckStatus, Text) -> IO a
exitWithStatus (OK, t) = putTxt t >> exitWith ExitSuccess
exitWithStatus (r, t) = putTxt t >> exitWith (ExitFailure $ fromEnum r)

putTxt :: Text -> IO ()
putTxt = (putStrLn . T.unpack)
