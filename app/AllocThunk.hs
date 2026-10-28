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
  putStrLn "  channel: delta of RTS allocated_bytes samples (+RTS -T required)"
  putStrLn "  allocX samples directly; allocGC samples after performGC at both ends"
  putStrLn ""

  putStrLn "--- allocX (raw counter delta; no GC forced) ---"
  t0 <- runK (start (both timeX allocX)) ()
  _ <- evaluate (sumTailLazy xs)
  (dtX, allocXVal) <- runK (stop (both timeX allocX)) t0
  printf "  time: %s  alloc: %s\n" (show dtX) (show allocXVal)

  putStrLn "--- allocGC (post-GC counter delta; not total payload allocation) ---"
  t1 <- runK (start (both timeX allocGC)) ()
  _ <- evaluate (sumTailLazy xs)
  (dtGC, allocGCVal) <- runK (stop (both timeX allocGC)) t1
  printf "  time: %s  alloc: %s\n" (show dtGC) (show allocGCVal)

  putStrLn "--- GC-start + allocX (raw counter delta; no final refresh) ---"
  performGC
  t1b <- runK (start (both timeX allocX)) ()
  _ <- evaluate (sumTailLazy xs)
  (dtGCb, allocGCb) <- runK (stop (both timeX allocX)) t1b
  printf "  time: %s  alloc: %s\n" (show dtGCb) (show allocGCb)

  putStrLn "--- sumMono baseline (allocGC post-GC counter delta) ---"
  t2 <- runK (start (both timeX allocGC)) ()
  _ <- evaluate (sumMono xs)
  (dtM, allocM) <- runK (stop (both timeX allocGC)) t2
  printf "  time: %s  alloc: %s\n" (show dtM) (show allocM)

  putStrLn ""
  putStrLn "readings: allocX can omit recent allocation not visible at its samples"
  putStrLn "          allocGC is not the 'real thunk allocation' total"
  putStrLn "          the runs reuse xs; these deltas are channel demonstrations"
