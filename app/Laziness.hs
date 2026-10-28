{-# LANGUAGE BlockArguments #-}

-- | Laziness stopwatch experiment.
--
-- Demonstrates lazy / WHNF / NF evaluation with both time and allocation
-- metering. Two meters run side-by-side: timeX and allocX. The space
-- channel reveals whether cost is compute or allocation.
--
-- Run: cabal run laziness
module Main where

import Circuit.Category (K (..), (.>))
import Circuit.Meter (Meter (..), both)
import Circuit.Meter.Space (Bytes (..), allocGC)
import Circuit.Meter.Stopwatch qualified as SW
import Circuit.Meter.Time (Nanos, timeX)
import Circuit.Syntax (eval)
import Circuit.Trace (FreeTrace)
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Text.Printf (printf)
import Prelude

n :: Int
n = 100000

-- | map+filter over a large range, returns a lazy list.
expensive :: Int -> [Int]
expensive n' = map (* 2) (filter even [1 .. n'])
{-# NOINLINE expensive #-}

-- | Force spine to WHNF.
whnf :: [a] -> [a]
whnf [] = []
whnf (x : xs) = x `seq` whnf xs
{-# NOINLINE whnf #-}

-- | Count elements (forces spine only).
count :: [a] -> Int
count = foldr (\_ acc -> acc + 1) 0
{-# NOINLINE count #-}

-- | Sum elements (forces everything).
total :: [Int] -> Int
total = foldr (+) 0
{-# NOINLINE total #-}

-- Meter that carries time and the delta between post-GC samples of the RTS
-- cumulative allocated_bytes counter.
timeSpace :: Meter (K IO) (Nanos, Bytes) (Nanos, Bytes)
timeSpace = both timeX allocGC

fmtTime :: Nanos -> String
fmtTime t
  | t < 1000 = show t ++ " ns"
  | t < 1000000 = printf "%.1f µs" (fromIntegral t / 1e3 :: Double)
  | otherwise = printf "%.1f ms" (fromIntegral t / 1e6 :: Double)

fmtBytes :: Bytes -> String
fmtBytes (Bytes b)
  | b < 1024 = show b ++ " B"
  | b < 1048576 = printf "%.1f KB" (fromIntegral b / 1024 :: Double)
  | otherwise = printf "%.1f MB" (fromIntegral b / 1048576 :: Double)

reportTime :: SW.Watches Nanos Nanos -> IO ()
reportTime w = do
  let laps = SW.allLaps w
  Map.toList laps
    & map
      ( \(label, ts) ->
          let sorted = List.sort ts
              p50 = sorted !! (length sorted `div` 2)
           in (label, p50)
      )
    & map (\(label, p50) -> printf "  %-12s %s\n" label (fmtTime p50))
    & sequence_

reportBoth :: SW.Watches (Nanos, Bytes) (Nanos, Bytes) -> IO ()
reportBoth w = do
  let laps = SW.allLaps w
  Map.toList laps
    & map
      ( \(label, ts) ->
          let sorted = List.sort ts
              p50 = sorted !! (length sorted `div` 2)
              (t, b) = p50
           in (label, t, b)
      )
    & map (\(label, t, b) -> printf "  %-12s %-10s  %s\n" label (fmtTime t) (fmtBytes b))
    & sequence_

(&) :: a -> (a -> b) -> b
(&) = flip ($)

main :: IO ()
main = do
  putStrLn "laziness stopwatch experiment"
  putStrLn $ "  list size: " ++ show n ++ " elements"
  putStrLn "  allocation: allocGC delta of RTS allocated_bytes after GC at both ends"
  putStrLn "  not a guaranteed total for the metered payload; +RTS -T required"
  putStrLn ""

  -- ==== Time-only pipelines ====

  putStrLn "=== time only ==="
  putStrLn ""
  putStrLn "  interval      time"
  putStrLn "  --------      ----"

  putStrLn "--- drop: create thunk, never force ---"
  (_, w1) <- runK (eval dropPipeline) ()
  reportTime w1

  putStrLn "--- WHNF: force spine, drop elements ---"
  (_, w2) <- runK (eval whnfPipeline) ()
  reportTime w2

  putStrLn "--- count: count spine only ---"
  (_, w3) <- runK (eval countPipeline) ()
  reportTime w3

  putStrLn "--- sum: force every element ---"
  (_, w4) <- runK (eval sumPipeline) ()
  reportTime w4

  -- ==== Time + space pipelines ====

  putStrLn ""
  putStrLn "=== time + allocation ==="
  putStrLn ""
  putStrLn "  interval      time        allocated"
  putStrLn "  --------      ----        ---------"

  putStrLn "--- drop: create thunk, never force ---"
  (_, w5) <- runK (eval dropPipelineTS) ()
  reportBoth w5

  putStrLn "--- WHNF: force spine, drop elements ---"
  (_, w6) <- runK (eval whnfPipelineTS) ()
  reportBoth w6

  putStrLn "--- count: count spine only ---"
  (_, w7) <- runK (eval countPipelineTS) ()
  reportBoth w7

  putStrLn "--- sum: force every element ---"
  (_, w8) <- runK (eval sumPipelineTS) ()
  reportBoth w8

  putStrLn ""
  putStrLn "interpretation:"
  putStrLn "  drop leaves the expensive list unevaluated"
  putStrLn "  WHNF, count, and sum force increasing portions of it"
  putStrLn "  allocation figures are post-GC counter deltas, not workload totals"
  putStrLn ""
  putStrLn "The allocation channel does not isolate allocation from compute."

-- ==== Time-only pipelines ====

dropPipeline :: FreeTrace (,) (K IO) () ((), SW.Watches Nanos Nanos)
dropPipeline =
  SW.start timeX "total"
    .> SW.carry (K (\() -> let _xs = expensive n in pure ()))
    .> SW.lap timeX "compute"
    .> SW.carry (K (\() -> pure ()))
    .> SW.stop timeX "total"

whnfPipeline :: FreeTrace (,) (K IO) () ((), SW.Watches Nanos Nanos)
whnfPipeline =
  SW.start timeX "total"
    .> SW.carry (K (\() -> evaluate (whnf (expensive n))))
    .> SW.lap timeX "compute"
    .> SW.carry (K (\_xs -> pure ()))
    .> SW.stop timeX "total"

countPipeline :: FreeTrace (,) (K IO) () ((), SW.Watches Nanos Nanos)
countPipeline =
  SW.start timeX "total"
    .> SW.carry (K (\() -> evaluate (count (expensive n))))
    .> SW.lap timeX "compute"
    .> SW.carry (K (\_c -> pure ()))
    .> SW.stop timeX "total"

sumPipeline :: FreeTrace (,) (K IO) () ((), SW.Watches Nanos Nanos)
sumPipeline =
  SW.start timeX "total"
    .> SW.carry (K (\() -> evaluate (total (expensive n))))
    .> SW.lap timeX "compute"
    .> SW.carry (K (\_s -> pure ()))
    .> SW.stop timeX "total"

-- ==== Time + space pipelines ====

dropPipelineTS :: FreeTrace (,) (K IO) () ((), SW.Watches (Nanos, Bytes) (Nanos, Bytes))
dropPipelineTS =
  SW.start timeSpace "total"
    .> SW.carry (K (\() -> let _xs = expensive n in pure ()))
    .> SW.lap timeSpace "compute"
    .> SW.carry (K (\() -> pure ()))
    .> SW.stop timeSpace "total"

whnfPipelineTS :: FreeTrace (,) (K IO) () ((), SW.Watches (Nanos, Bytes) (Nanos, Bytes))
whnfPipelineTS =
  SW.start timeSpace "total"
    .> SW.carry (K (\() -> evaluate (whnf (expensive n))))
    .> SW.lap timeSpace "compute"
    .> SW.carry (K (\_xs -> pure ()))
    .> SW.stop timeSpace "total"

countPipelineTS :: FreeTrace (,) (K IO) () ((), SW.Watches (Nanos, Bytes) (Nanos, Bytes))
countPipelineTS =
  SW.start timeSpace "total"
    .> SW.carry (K (\() -> evaluate (count (expensive n))))
    .> SW.lap timeSpace "compute"
    .> SW.carry (K (\_c -> pure ()))
    .> SW.stop timeSpace "total"

sumPipelineTS :: FreeTrace (,) (K IO) () ((), SW.Watches (Nanos, Bytes) (Nanos, Bytes))
sumPipelineTS =
  SW.start timeSpace "total"
    .> SW.carry (K (\() -> evaluate (total (expensive n))))
    .> SW.lap timeSpace "compute"
    .> SW.carry (K (\_s -> pure ()))
    .> SW.stop timeSpace "total"
