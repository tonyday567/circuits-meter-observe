{-# LANGUAGE BlockArguments #-}

-- | Machina showcase: pipeline-level performance metering.
module Main where

import Circuit.Category ((.>))
import Circuit.Layer (run)
import Circuit.Meter.Stopwatch
import Circuit.Meter.Time (Nanos, timeX)
import Control.Arrow (Kleisli (..), runKleisli)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Prelude hiding (id, (.))
import Text.Printf (printf)

-- ---------------------------------------------------------------------------
-- Pipeline stages
-- ---------------------------------------------------------------------------

stageA :: Kleisli IO Int Int
stageA = Kleisli \x -> do
  let !r = sum [0 .. x]
  pure r
{-# NOINLINE stageA #-}

stageB :: Kleisli IO Int Int
stageB = Kleisli \x -> do
  let !r = x * x
  pure r
{-# NOINLINE stageB #-}

stageC :: Kleisli IO Int String
stageC = Kleisli \n -> pure (show n <> "!")
{-# NOINLINE stageC #-}

-- ---------------------------------------------------------------------------
-- Pipeline
-- ---------------------------------------------------------------------------

meterPipeline :: Int -> IO (String, Watches Nanos Nanos)
meterPipeline n = runKleisli (run pipeline) n
  where
    pipeline =
      start timeX "total"
        .> carry stageA
        .> lap timeX "stageA"
        .> carry stageB
        .> lap timeX "stageB"
        .> carry stageC
        .> stop timeX "total"

-- ---------------------------------------------------------------------------
-- Main
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "circuits-meter-observe: machina showcase"
  putStrLn ""

  (result, watches) <- meterPipeline 5000

  putStrLn $ "result: " ++ result
  putStrLn ""

  putStrLn "timing log:"
  Map.toList (allLaps watches)
    & map (\(label, laps) ->
      let sorted = List.sort laps
          p50 = sorted !! (length sorted `div` 2)
       in (label, p50))
    & map (\(label, p50) ->
      let (v, u) = scaleNanos p50
       in printf "  %-10s %6.1f %s\n" label v u)
    & sequence_

  putStrLn ""
  putStrLn "machina pattern: start → carry stage → lap → carry stage → ... → stop"
  putStrLn "each lap records an interval; the log travels on an (,) wire."
  putStrLn "Done."

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

(&) :: a -> (a -> b) -> b
(&) = flip ($)

scaleNanos :: Nanos -> (Double, String)
scaleNanos n
  | n < 1000 = (fromIntegral n, "ns")
  | n < 1000000 = (fromIntegral n / 1e3, "µs")
  | otherwise = (fromIntegral n / 1e6, "ms")
