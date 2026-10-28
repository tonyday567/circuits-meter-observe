{-# LANGUAGE BlockArguments #-}

-- | Allocation probe with GC-forced boundaries.
-- Forces a GC before start and before stop so allocated_bytes is current.
module Main where

import Circuit.Meter (Meter (..))
import Circuit.Meter.Space (Bytes (..))
import Control.Arrow (Kleisli (..), runKleisli)
import Control.Exception (evaluate)
import GHC.Stats (allocated_bytes, getRTSStats)
import System.Mem (performGC)
import Text.Printf (printf)
import Prelude

-- | Meter that samples the RTS cumulative @allocated_bytes@ counter after
-- 'performGC' at each boundary. The reported delta is the movement between
-- those two post-GC samples, not a guaranteed total for the metered payload.
allocGC :: Meter (Kleisli IO) Bytes Bytes
allocGC =
  Meter
    { start = Kleisli \_ -> performGC >> fmap (Bytes . allocated_bytes) getRTSStats,
      stop = Kleisli \s -> do
        performGC
        s' <- fmap (Bytes . allocated_bytes) getRTSStats
        pure (s' - s)
    }

-- | Allocate a list and sum it.
work :: Int -> IO Int
work n = do
  let !xs = [1 .. n :: Int]
      !s = sum xs
  pure s

main :: IO ()
main = do
  putStrLn "allocation probe (allocGC post-GC counter deltas)"
  putStrLn "channel: delta of RTS allocated_bytes after performGC at each boundary"
  putStrLn "not a guaranteed total for the metered payload; +RTS -T required"
  putStrLn ""

  -- Small allocation (fits in nursery)
  putStrLn "--- small (n=1000) ---"
  t0 <- runKleisli (start allocGC) ()
  _ <- work 1000
  dt <- runKleisli (stop allocGC) t0
  printf "  allocGC-delta: %s\n" (show dt)

  -- Medium
  putStrLn "--- medium (n=100000) ---"
  t1 <- runKleisli (start allocGC) ()
  _ <- work 100000
  dt1 <- runKleisli (stop allocGC) t1
  printf "  allocGC-delta: %s\n" (show dt1)

  -- Large
  putStrLn "--- large (n=1000000) ---"
  t2 <- runKleisli (start allocGC) ()
  _ <- work 1000000
  dt2 <- runKleisli (stop allocGC) t2
  printf "  allocGC-delta: %s\n" (show dt2)

  -- Compare: pure arithmetic control
  putStrLn "--- pure arithmetic control ---"
  t3 <- runKleisli (start allocGC) ()
  let !s = sum [1 .. 1000000 :: Int]
  _ <- evaluate s
  dt3 <- runKleisli (stop allocGC) t3
  printf "  allocGC-delta: %s\n" (show dt3)
