{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

-- | Benchmark: performance measurement as a Circuit.
--
-- Five measurements, all using the public runners from
-- 'Circuit.Meter.Time' rather than hand-rolled nanosecond brackets:
--
--   1. clock overhead  — 'timeX' bracketing a no-op
--   2. whileM_         — 'timesK' around the IORef loop
--   3. trace-delim     — 'timesK' around the Either loop
--   4. meterAction-loop — same loop measured with 'Circuit.Meter.meterAction'
--   5. both            — simultaneous time + space via 'both'
--
-- Usage:
--   perf-bench --runs 100000 --warmup 1000
module Main where

import Circuit.Meter
import Circuit.Meter.Space
import Circuit.Meter.Time
import Circuit.Channel (trace)
import Control.Arrow hiding (loop)
import Data.IORef
import Data.List qualified as List
import GHC.Stats
import Options.Applicative
import Prelude hiding (id, (.))

-- ---------------------------------------------------------------------------
-- CLI
-- ---------------------------------------------------------------------------

data Config = Config
  { cfgRuns :: !Int,
    cfgWarmup :: !Int,
    cfgTraceTarget :: !Int
  }

configP :: Parser Config
configP =
  Config
    <$> option auto (long "runs" <> short 'n' <> value 100000 <> help "Number of outer iterations")
    <*> option auto (long "warmup" <> short 'w' <> value 1000 <> help "Warmup iterations")
    <*> option auto (long "trace-target" <> short 't' <> value 1000 <> help "Inner loop count for trace/whileM")

-- ---------------------------------------------------------------------------
-- Reporting
-- ---------------------------------------------------------------------------

fmt :: Nanos -> String
fmt n = let (v, u) = scaleNanos n in show (round v :: Int) <> u

scaleNanos :: Nanos -> (Double, String)
scaleNanos n
  | n < 1000 = (fromIntegral n, "ns")
  | n < 1000000 = (fromIntegral n / 1e3, "µs")
  | otherwise = (fromIntegral n / 1e6, "ms")

report :: String -> [Nanos] -> IO ()
report name xs = do
  let sorted = List.sort xs
      n = length xs
      p10 = sorted !! (n `div` 10)
      p50 = sorted !! (n `div` 2)
      p90 = sorted !! (n * 9 `div` 10)
      avg = sum sorted `div` fromIntegral n
  putStrLn $
    name
      <> ": p10="
      <> fmt p10
      <> " p50="
      <> fmt p50
      <> " p90="
      <> fmt p90
      <> " avg="
      <> fmt avg

-- ---------------------------------------------------------------------------
-- Benchmark 1: clock overhead
-- ---------------------------------------------------------------------------

benchClock :: Config -> IO [Nanos]
benchClock cfg = do
  let n = cfgRuns cfg
  (ts, _) <- runKleisli (timesK 0 n timeX (Kleisli (\() -> pure ()))) ()
  pure ts

-- ---------------------------------------------------------------------------
-- Benchmark 2: whileM_ control group
-- ---------------------------------------------------------------------------

countIORef :: Int -> IO Int
countIORef target = do
  ref <- newIORef 0
  let loop = do
        n <- readIORef ref
        if n >= target
          then pure n
          else writeIORef ref (n + 1) >> loop
  loop
{-# NOINLINE countIORef #-}

benchWhileM :: Config -> IO [Nanos]
benchWhileM cfg = do
  let target = cfgTraceTarget cfg
      n = cfgRuns cfg
  (ts, _) <- runKleisli (timesK 0 n timeX (Kleisli (\() -> countIORef target))) ()
  pure ts

-- ---------------------------------------------------------------------------
-- Benchmark 3: delimited continuation trace
-- ---------------------------------------------------------------------------

countTrace :: Int -> Kleisli IO (Either Int ()) (Either Int Int)
countTrace target = Kleisli \case
  Right () -> countUp 0
  Left n -> countUp n
  where
    countUp n
      | n >= target = pure (Right n)
      | otherwise = pure (Left (n + 1))
{-# NOINLINE countTrace #-}

runTrace :: Int -> IO Int
runTrace n = runKleisli (trace (countTrace n)) ()
{-# NOINLINE runTrace #-}

benchTrace :: Config -> IO [Nanos]
benchTrace cfg = do
  let target = cfgTraceTarget cfg
      n = cfgRuns cfg
  (ts, _) <- runKleisli (timesK 0 n timeX (Kleisli (\() -> runTrace target))) ()
  pure ts

-- ---------------------------------------------------------------------------
-- Benchmark 4: meterA on the trace loop
-- ---------------------------------------------------------------------------

-- | The trace loop wrapped in a 'Meter'. 'meterAction timeX' builds a
-- circuit that meters each call; 'timesK' iterates it via 'reifyC'.
benchMeterK :: Config -> IO [Nanos]
benchMeterK cfg = do
  let target = cfgTraceTarget cfg
      n = cfgRuns cfg
      kaction = Kleisli (\() -> runTrace target)
  (ts, _r) <- runKleisli (timesK 0 n timeX kaction) ()
  pure ts

-- ---------------------------------------------------------------------------
-- Benchmark 5: simultaneous time + space (single shot)
-- ---------------------------------------------------------------------------

benchBoth :: Config -> IO ()
benchBoth cfg = do
  enabled <- getRTSStatsEnabled
  if not enabled
    then putStrLn "time+space: skipped (enable with +RTS -T)"
    else do
      let target = cfgTraceTarget cfg
          meterBoth = both timeX allocX
          kaction = Kleisli (\() -> runTrace target)
      ((dt, alloc), _r) <- runKleisli (reifyC (meterAction meterBoth kaction)) ()
      putStrLn $
        "time+space: time="
          <> fmt dt
          <> " alloc="
          <> show (unbytes alloc)
          <> "B"

-- ---------------------------------------------------------------------------
-- Main
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  cfg <- execParser (info (configP <**> helper) fullDesc)
  let runs = cfgRuns cfg
      warm = cfgWarmup cfg
      target = cfgTraceTarget cfg

  putStrLn $ "perf-bench: runs=" <> show runs <> " warmup=" <> show warm <> " trace-target=" <> show target
  putStrLn ""

  -- clock overhead
  putStrLn "1. clock overhead"
  warmup warm
  cs <- benchClock cfg
  report "clock" cs
  putStrLn ""

  -- whileM_ control
  putStrLn "2. whileM_ (IORef control)"
  warmup warm
  ws <- benchWhileM cfg
  report "whileM_" ws
  putStrLn ""

  -- trace-delim
  putStrLn "3. trace-delim (delimited continuations)"
  warmup warm
  ts <- benchTrace cfg
  report "trace-delim" ts
  putStrLn ""

  -- meterAction on trace
  putStrLn "4. meterAction + timesK (circuit perf API)"
  ms <- benchMeterK cfg
  report "meterAction" ms
  putStrLn ""

  -- simultaneous time + space (single shot, not repeated)
  putStrLn "5. both timeX + allocX (single shot)"
  benchBoth cfg
  putStrLn ""

  putStrLn "Done."
