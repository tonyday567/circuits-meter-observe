{-# LANGUAGE BlockArguments #-}

-- | A typed lazy knot beside its hand-tied control.
--
-- @time@ compares repeated calls in one process. @work VARIANT N@ performs one
-- call for whole-process allocation measurement with @+RTS -s@; @empty@ is the
-- harness control. Keeping allocation outside the timer stops GC and clock
-- overhead from contaminating the workload.
--
-- > cabal run knot-demo -- time
-- > cabal run knot-demo -- time free 20000
-- > cabal run knot-demo -- work direct 20000 +RTS -s
module Main where

import Circuit.Meter.Time (Nanos, ticks)
import Circuit.Syntax (Syntax (Lift), eval)
import Circuit.Trace (FreeTrace)
import Circuit.Traced (trace)
import Data.List (sort)
import System.Environment (getArgs)
import System.Exit (die)
import Text.Printf (printf)
import Prelude

runs :: Int
runs = 100

sizes :: [Int]
sizes = [1000, 5000, 20000]

knotStep :: Int -> ([Int], ()) -> ([Int], [Int])
knotStep n (ns, ()) = (n : map (+ 1) ns, ns)

emptyKnot :: Int -> Int
emptyKnot n = 2 * n
{-# NOINLINE emptyKnot #-}

at :: Int -> [a] -> a
at n xs = case drop n xs of
  x : _ -> x
  [] -> error "at: finite list"

directKnot :: Int -> Int
directKnot n = at n ns
  where
    ~(ns, _) = knotStep n (ns, ())
{-# NOINLINE directKnot #-}

tracedKnot :: Int -> Int
tracedKnot n = at n (trace (knotStep n) ())
{-# NOINLINE tracedKnot #-}

freeKnot :: Int -> Int
freeKnot n = at n (eval knot ())
  where
    knot :: FreeTrace (,) (->) () [Int]
    knot = trace (Lift (knotStep n))
{-# NOINLINE freeKnot #-}

quantile :: Double -> [Nanos] -> Nanos
quantile q xs =
  let ordered = sort xs
      i = floor (q * fromIntegral (length ordered - 1))
   in ordered !! i

measure :: String -> (Int -> Int) -> Int -> IO ()
measure label f n = do
  (times, timedResult) <- ticks runs f n
  let p10 = quantile 0.1 times
      p50 = quantile 0.5 times
      p90 = quantile 0.9 times
      nsPerElement = fromIntegral p50 / fromIntegral n :: Double
  if timedResult == 2 * n
    then pure ()
    else error (label <> ": wrong result")
  printf
    "%-7s n=%-5d time p10=%d p50=%d p90=%d ns; p50/n=%.2f ns\n"
    label
    n
    p10
    p50
    p90
    nsPerElement

variants :: [(String, Int -> Int)]
variants =
  [ ("direct", directKnot),
    ("traced", tracedKnot),
    ("free", freeKnot)
  ]

timeAll :: IO ()
timeAll = do
  printf "lazy-knot timing: runs=%d; ticks warmups=100\n" runs
  putStrLn "corpus: ns = n : map (+1) ns; force element n; expected result = 2n"
  mapM_ (\n -> mapM_ (\(label, f) -> measure label f n) variants >> putStrLn "") sizes

timeOne :: String -> Int -> IO ()
timeOne name n = case lookup name variants of
  Just f -> measure name f n
  Nothing -> die ("unknown variant: " <> name)

workOne :: String -> Int -> IO ()
workOne "empty" n = print (emptyKnot n)
workOne name n = case lookup name variants of
  Just f -> print (f n)
  Nothing -> die ("unknown variant: " <> name)

main :: IO ()
main =
  getArgs >>= \case
    ["time"] -> timeAll
    ["time", name, n] -> timeOne name (read n)
    ["work", name, n] -> workOne name (read n)
    _ -> die "usage: knot-demo time [VARIANT N] | knot-demo work empty|direct|traced|free N"
