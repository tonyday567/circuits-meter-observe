{-# LANGUAGE BlockArguments #-}

-- | @Data.List.nub@ as a quadratic reference shape, measured with circuits-meter.
--
-- nub keeps the first occurrence of each element via a nested scan — O(n²).
-- On all-distinct input @[1..n]@ it does maximum work, giving a clean curve to
-- fit @time = c * n²@ against. Set- and sort-based dedup are O(n log n) controls.
--
-- Run with:
--   cabal run nub -- --runs 50
--   cabal run nub -- --runs 50 --sizes 100,200,500,1000,2000,5000,10000
module Main where

import Circuit.Meter.Time (ticks)
import Data.List (group, nub, sort)
import Data.List qualified as List
import Data.Set qualified as Set
import Options.Applicative
import Text.Printf (printf)
import Prelude

-- ---------------------------------------------------------------------------
-- CLI
-- ---------------------------------------------------------------------------

data Config = Config
  { cfgRuns :: !Int,
    cfgSizes :: [Int]
  }

configP :: Parser Config
configP =
  Config
    <$> option auto (long "runs" <> short 'r' <> value 50 <> help "Measurement iterations per size")
    <*> option
      (eitherReader parseSizes)
      ( long "sizes"
          <> value [100, 200, 500, 1000, 2000, 5000, 10000]
          <> help "Comma-separated input sizes"
      )

parseSizes :: String -> Either String [Int]
parseSizes = traverse rd . splitOn ','
  where
    rd s = case reads s of [(n, "")] -> Right n; _ -> Left ("bad size: " <> s)
    splitOn c s = case break (== c) s of
      (a, []) -> [a]
      (a, _ : rest) -> a : splitOn c rest

-- ---------------------------------------------------------------------------
-- Subjects
-- ---------------------------------------------------------------------------

nubList :: [Int] -> [Int]
nubList = nub -- O(n²), classic nested scan
{-# NOINLINE nubList #-}

nubSet :: [Int] -> [Int]
nubSet = Set.toList . Set.fromList -- O(n log n), balanced tree
{-# NOINLINE nubSet #-}

nubSort :: [Int] -> [Int]
nubSort = concatMap (take 1) . group . sort -- O(n log n), sort then group
{-# NOINLINE nubSort #-}

mkList :: Int -> [Int]
mkList n = [1 .. n]
{-# NOINLINE mkList #-}

-- ---------------------------------------------------------------------------
-- Measurement
-- ---------------------------------------------------------------------------

-- | p50 (median) nanoseconds for @runs@ timings of @f@ on @[1..n]@.
p50Of :: Int -> ([Int] -> [Int]) -> Int -> IO Integer
p50Of runs f n = do
  let !xs = mkList n
  (ts, _) <- ticks runs f xs
  let sorted = List.sort ts
  pure (sorted !! (length sorted `div` 2))

fmt :: Integer -> String
fmt n
  | n < 1000 = show n <> "ns"
  | n < 1000000 = printf "%.1fµs" (fromIntegral n / 1e3 :: Double)
  | otherwise = printf "%.2fms" (fromIntegral n / 1e6 :: Double)

-- ---------------------------------------------------------------------------
-- Quadratic fit: time = c * n², least squares through the origin
-- ---------------------------------------------------------------------------

-- | c = Σ(t·n²) / Σ(n⁴), and the R² of the fit.
fitQuadratic :: [(Int, Integer)] -> (Double, Double)
fitQuadratic pts = (c, r2)
  where
    ns2 = [fromIntegral (n * n) | (n, _) <- pts] :: [Double]
    ts = [fromIntegral t | (_, t) <- pts] :: [Double]
    c = sum (zipWith (*) ts ns2) / sum (map (^ (2 :: Int)) ns2)
    tbar = sum ts / fromIntegral (length ts)
    ssTot = sum [(t - tbar) ^ (2 :: Int) | t <- ts]
    ssRes = sum [(t - c * x) ^ (2 :: Int) | (x, t) <- zip ns2 ts]
    r2 = 1 - ssRes / ssTot

-- ---------------------------------------------------------------------------
-- Main
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  cfg <- execParser (info (configP <**> helper) fullDesc)
  let runs = cfgRuns cfg
      sizes = cfgSizes cfg

  printf "nub reference shape: runs=%d sizes=%s\n\n" runs (show sizes)

  printf "%-8s %12s %12s %12s %14s\n" "n" "nub(p50)" "nubSet(p50)" "nubSort(p50)" "nub/nubSet"
  nubPts <-
    mapM
      ( \n -> do
          tn <- p50Of runs nubList n
          ts <- p50Of runs nubSet n
          to <- p50Of runs nubSort n
          let ratio = fromIntegral tn / fromIntegral ts :: Double
          printf "%-8d %12s %12s %12s %13.1fx\n" n (fmt tn) (fmt ts) (fmt to) ratio
          pure (n, tn)
      )
      sizes

  putStrLn ""
  let (c, r2) = fitQuadratic nubPts
  printf "quadratic fit  time = c * n²   c = %.3f ns   R² = %.6f\n" c r2
  putStrLn ""
  printf "%-8s %12s %12s %10s\n" "n" "actual" "predicted" "residual"
  mapM_
    ( \(n, t) -> do
        let pred_ = c * fromIntegral (n * n)
            resid = (fromIntegral t - pred_) / fromIntegral t * 100 :: Double
        printf "%-8d %12s %12s %+9.1f%%\n" n (fmt t) (fmt (round pred_ :: Integer)) resid
    )
    nubPts

  putStrLn "\nDone."
