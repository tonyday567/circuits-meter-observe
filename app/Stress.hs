{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Stress test: time + allocation for composition chains.
module Main where

import Circuit.Category (Category (..), K (..))
import Circuit.Hyper (Hyper, baseHyper, lift, observe, push, pattern Hyper)
import Circuit.Meter (Meter (..), both)
import Circuit.Meter.Space (Bytes (..), allocGC)
import Circuit.Meter.Time (Nanos, timeX)
import Circuit.Syntax (Syntax (Lift), eval)
import Circuit.Trace (FreeTrace)
import Control.Monad (forM_)
import Data.List (foldl')
import System.Mem (performGC)
import Text.Printf (printf)
import Prelude hiding (id, (.))

-- | Build a FreeTrace composition of N Lifts.
loopChain :: Int -> FreeTrace (,) (->) Int Int
loopChain 0 = Lift (+ 1)
loopChain n = loopChain (n - 1) . Lift (+ 1)

-- | Build a Hyper composition of N lifts.
hyperChain :: Int -> Hyper Int Int
hyperChain 0 = lift (+ 1)
hyperChain n = hyperChain (n - 1) . lift (+ 1)

-- | Build a Hyper push chain.
hyperPushChain' :: Int -> Hyper Int Int
hyperPushChain' n = go n (baseHyper 0)
  where
    go 0 h = push (+ 1) h; go i h = go (i - 1) (push (+ 1) h)

measure :: String -> (Int -> a) -> (a -> Int) -> [Int] -> IO ()
measure label build runF sizes = do
  putStrLn $ "\n=== " ++ label ++ " ==="
  printf "%-8s %12s %10s %12s\n" ("n" :: String) ("time" :: String) ("ns/elem" :: String) ("alloc" :: String)
  forM_ sizes $ \n -> do
    let chain = build n
    performGC
    t0 <- runK (start (both timeX allocGC)) ()
    let !result = runF chain
    (dt, Bytes alloc) <- runK (stop (both timeX allocGC)) t0
    let nsPerElem = fromIntegral dt / fromIntegral n :: Double
    printf "%-8d %10d ns %10.1f %10d\n" n dt nsPerElem alloc

main :: IO ()
main = do
  putStrLn "allocation: allocGC delta of RTS allocated_bytes after GC at both ends"
  putStrLn "not a guaranteed total for the metered payload; +RTS -T required"
  let sizes = [100, 500, 1000, 5000, 10000]

  measure "Loop compose" loopChain (`eval` 0) sizes
  measure "Hyper compose" hyperChain (`observe` 0) sizes
  measure "Hyper push chain" hyperPushChain' (`observe` 0) sizes

  putStrLn "\n--- direct function composition (baseline) ---"
  forM_ sizes $ \n -> do
    performGC
    t0 <- runK (start (both timeX allocGC)) ()
    let f = (+ 1) :: Int -> Int
        fs = replicate n f
        !result = foldr (.) id fs 0
    (dt, Bytes alloc) <- runK (stop (both timeX allocGC)) t0
    let nsPerElem = fromIntegral dt / fromIntegral n :: Double
    printf "%-8d %10d ns %10.1f %10d\n" n dt nsPerElem alloc
