{-# LANGUAGE BlockArguments #-}

-- | Allocation probe with GC-forced boundaries.
-- Forces a GC before start and before stop so allocated_bytes is current.
module Main where

import Circuit.Meter (Meter (..))
import Circuit.Meter.Space (Bytes (..))
import Control.Arrow (Kleisli (..), runKleisli)
import GHC.Stats (allocated_bytes, getRTSStats)
import System.Mem (performGC)
import Control.Exception (evaluate)
import Prelude
import Text.Printf (printf)

-- | Meter that forces GC before reading the allocation counter.
-- This gives accurate per-interval allocation at the cost of GC overhead.
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
  putStrLn "allocation probe (GC-forced boundaries)"
  putStrLn ""

  -- Small allocation (fits in nursery)
  putStrLn "--- small (n=1000) ---"
  t0 <- runKleisli (start allocGC) ()
  _ <- work 1000
  dt <- runKleisli (stop allocGC) t0
  printf "  alloc: %s\n" (show dt)

  -- Medium
  putStrLn "--- medium (n=100000) ---"
  t1 <- runKleisli (start allocGC) ()
  _ <- work 100000
  dt1 <- runKleisli (stop allocGC) t1
  printf "  alloc: %s\n" (show dt1)

  -- Large
  putStrLn "--- large (n=1000000) ---"
  t2 <- runKleisli (start allocGC) ()
  _ <- work 1000000
  dt2 <- runKleisli (stop allocGC) t2
  printf "  alloc: %s\n" (show dt2)

  -- Compare: no allocation (pure arithmetic)
  putStrLn "--- none (pure arithmetic) ---"
  t3 <- runKleisli (start allocGC) ()
  let !s = sum [1 .. 1000000 :: Int]
  _ <- evaluate s
  dt3 <- runKleisli (stop allocGC) t3
  printf "  alloc: %s\n" (show dt3)
