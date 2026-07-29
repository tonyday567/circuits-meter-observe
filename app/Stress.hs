{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
-- | Stress test: time + allocation for composition chains.
module Main where

import Circuit.Category (Category (..))
import Circuit.Hyper (Hyper, pattern Hyper, lift, observe, push, base)
import Circuit.Layer (run)
import Circuit.Loop (Loop (..))
import Circuit.Meter (Meter (..), both)
import Circuit.Meter.Space (Bytes (..), allocGC)
import Circuit.Meter.Time (Nanos, timeX)
import Control.Arrow (Kleisli (..), runKleisli)
import Control.Monad (forM_)
import Data.List (foldl')
import Prelude hiding (id, (.))
import System.Mem (performGC)
import Text.Printf (printf)

-- | Build a Loop composition of N Lifts.
loopChain :: Int -> Loop (,) (->) Int Int
loopChain 0 = Lift (+1)
loopChain n = loopChain (n - 1) . Lift (+1)

-- | Build a Hyper composition of N lifts.
hyperChain :: Int -> Hyper Int Int
hyperChain 0 = lift (+1)
hyperChain n = hyperChain (n - 1) . lift (+1)

-- | Build a Hyper push chain.
hyperPushChain' :: Int -> Hyper Int Int
hyperPushChain' n = go n (base 0)
  where go 0 h = push (+1) h; go i h = go (i - 1) (push (+1) h)

measure :: String -> (Int -> a) -> (a -> Int) -> [Int] -> IO ()
measure label build runF sizes = do
  putStrLn $ "\n=== " ++ label ++ " ==="
  printf "%-8s %12s %10s %12s\n" ("n" :: String) ("time" :: String) ("ns/elem" :: String) ("alloc" :: String)
  forM_ sizes $ \n -> do
    let chain = build n
    performGC
    t0 <- runKleisli (start (both timeX allocGC)) ()
    let !result = runF chain
    (dt, Bytes alloc) <- runKleisli (stop (both timeX allocGC)) t0
    let nsPerElem = fromIntegral dt / fromIntegral n :: Double
    printf "%-8d %10d ns %10.1f %10d\n" n dt nsPerElem alloc

main :: IO ()
main = do
  let sizes = [100, 500, 1000, 5000, 10000]

  measure "Loop compose" loopChain (\l -> run l 0) sizes
  measure "Hyper compose" hyperChain (\h -> observe h 0) sizes
  measure "Hyper push chain" hyperPushChain' (\h -> observe h 0) sizes

  putStrLn "\n--- direct function composition (baseline) ---"
  forM_ sizes $ \n -> do
    performGC
    t0 <- runKleisli (start (both timeX allocGC)) ()
    let f = (+1) :: Int -> Int
        fs = replicate n f
        !result = foldr (.) id fs 0
    (dt, Bytes alloc) <- runKleisli (stop (both timeX allocGC)) t0
    let nsPerElem = fromIntegral dt / fromIntegral n :: Double
    printf "%-8d %10d ns %10.1f %10d\n" n dt nsPerElem alloc
