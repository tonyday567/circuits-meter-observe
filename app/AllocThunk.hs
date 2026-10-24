{-# LANGUAGE BlockArguments #-}

-- | allocX vs allocGC: thunk buildup in sumTailLazy.
module Main where

import Circuit.Category (K (..))
import Circuit.Meter (Meter (..), both)
import Circuit.Meter.Space (Bytes (..), allocGC, allocX)
import Circuit.Meter.Time (Nanos, timeX)
import Control.Exception (evaluate)
import Data.List (foldl')
import GHC.Stats (allocated_bytes)
import System.Mem (performGC)
import Text.Printf (printf)
import Prelude

sumTailLazy :: (Num a) => [a] -> a
sumTailLazy = go 0
  where
    go acc [] = acc; go acc (x : xs) = go (x + acc) xs
{-# NOINLINE sumTailLazy #-}

sumMono :: [Int] -> Int
sumMono = foldl' (+) 0
{-# NOINLINE sumMono #-}

main :: IO ()
main = do
  let n = 100000 :: Int
      xs = [1 .. n]

  putStrLn "allocX vs allocGC: sumTailLazy (thunk buildup)"
  putStrLn $ "  n = " ++ show n
  putStrLn ""

  -- allocX (no GC forcing — counter may be stale)
  putStrLn "--- allocX (GC-time snapshot, may miss nursery) ---"
  t0 <- runK (start (both timeX allocX)) ()
  _ <- evaluate (sumTailLazy xs)
  (dtX, allocXVal) <- runK (stop (both timeX allocX)) t0
  printf "  time: %s  alloc: %s\n" (show dtX) (show allocXVal)

  putStrLn "--- allocGC (GC-forced, accurate) ---"
  t1 <- runK (start (both timeX allocGC)) ()
  _ <- evaluate (sumTailLazy xs)
  (dtGC, allocGCVal) <- runK (stop (both timeX allocGC)) t1
  printf "  time: %s  alloc: %s\n" (show dtGC) (show allocGCVal)

  -- GC at start only, then allocX (best of both)
  putStrLn "--- GC-start + allocX (correct: GC reset, no stop GC) ---"
  performGC
  t1b <- runK (start (both timeX allocX)) ()
  _ <- evaluate (sumTailLazy xs)
  (dtGCb, allocGCb) <- runK (stop (both timeX allocX)) t1b
  printf "  time: %s  alloc: %s\n" (show dtGCb) (show allocGCb)

  putStrLn "--- sumMono baseline (allocGC) ---"
  t2 <- runK (start (both timeX allocGC)) ()
  _ <- evaluate (sumMono xs)
  (dtM, allocM) <- runK (stop (both timeX allocGC)) t2
  printf "  time: %s  alloc: %s\n" (show dtM) (show allocM)

  putStrLn ""
  putStrLn "expected: allocX may report 0 for sumTailLazy (counter stale)"
  putStrLn "          allocGC reports the real thunk allocation"
  putStrLn "          sumMono alloc ~ 0 (foldl' is strict, no thunks)"
