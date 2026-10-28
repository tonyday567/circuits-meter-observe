-- | Word-count pipeline with stopwatch/interval metering.
--
-- The timing log is a first-class wire: it travels alongside the payload on
-- the (,) tensor. Every composition point can be a named marker.
module Main where

import Circuit.Category (K (..), (.>))
import Circuit.Meter (firstK)
import Circuit.Meter.Stopwatch
import Circuit.Meter.Time (Nanos, timeX)
import Circuit.Syntax (Syntax (Lift), eval)
import Circuit.Trace (FreeTrace)
import Circuit.Traced (trace)
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Data.Bool (bool)
import Data.Char (toLower)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Ord (Down (..))
import Numeric (showFFloat)
import System.IO (Handle, IOMode (ReadMode), hClose, hGetLine, hIsEOF, openFile)

-- ---------------------------------------------------------------------------
-- Pure components
-- ---------------------------------------------------------------------------

splitWords :: String -> [String]
splitWords = words

lowerWords :: [String] -> [String]
lowerWords = map (map toLower)

noEmpties :: [String] -> [String]
noEmpties = filter (not . null)

insertCount :: Map String Int -> String -> Map String Int
insertCount m w = Map.insertWith (+) w (1 :: Int) m

foldCounts :: [String] -> Map String Int -> Map String Int
foldCounts = flip (foldl' insertCount)

fmtTable :: Map String Int -> String
fmtTable = unlines . map fmt . take 5 . sortOn (Down . snd) . Map.toList
  where
    fmt (w, c) = w <> ": " <> show c

-- ---------------------------------------------------------------------------
-- FreeTrace primitives — payload-neutral, no closures
-- ---------------------------------------------------------------------------

openf :: FreeTrace t (K IO) FilePath Handle
openf = Lift (K (`openFile` ReadMode))

closef :: FreeTrace t (K IO) Handle ()
closef = Lift (K hClose)

-- | Close the Handle and keep the paired value.
post :: K IO (Handle, a) a
post = firstK (eval (closef :: FreeTrace (,) (K IO) Handle ())) .> K (pure . snd)

-- | Force the map component of the loop result so the read stage pays for
-- the counting work rather than deferring it to formatting.
forceMap :: K IO (Handle, Map String Int) (Handle, Map String Int)
forceMap = K $ \(h, m) -> do
  !m' <- evaluate (force m)
  pure (h, m')

formatf :: FreeTrace t (K IO) (Map String Int) String
formatf = Lift (K (pure . fmtTable))

-- | Forced variant so the stopwatch measures the actual formatting work.
formatfForced :: FreeTrace t (K IO) (Map String Int) String
formatfForced = Lift (K (evaluate . force . fmtTable))

-- ---------------------------------------------------------------------------
-- FreeTrace body — Either tensor, Handle rides the feedback wire
-- ---------------------------------------------------------------------------

readAndCount :: FreeTrace Either (K IO) Handle (Handle, Map String Int)
readAndCount = trace (Lift (K step))
  where
    step (Left (h, acc)) =
      hIsEOF h
        >>= bool
          ( hGetLine h >>= \line ->
              let ws = splitWords line
                  ls = lowerWords ws
                  ns = noEmpties ls
                  acc' = foldCounts ns acc
               in pure (Left (h, acc'))
          )
          (pure (Right (h, acc)))
    step (Right h) =
      pure (Left (h, Map.empty))

-- ---------------------------------------------------------------------------
-- Stopwatch pipeline
-- ---------------------------------------------------------------------------

-- | Top-level word-count pipeline with interval markers.
wordPipeline :: FreeTrace (,) (K IO) FilePath (String, Watches Nanos Nanos)
wordPipeline =
  start timeX "total"
    .> carryT (openf :: FreeTrace (,) (K IO) FilePath Handle)
    .> lap timeX "opened"
    .> carryT readAndCount
    .> carry forceMap
    .> lap timeX "read"
    .> carry post
    .> lap timeX "closed"
    .> carryT (formatfForced :: FreeTrace (,) (K IO) (Map String Int) String)
    .> lap timeX "format"
    .> stop timeX "total"

-- | Run the pipeline and print the word table plus the timing log.
wordCount :: FilePath -> IO ()
wordCount path = do
  (output, ws) <- runK (eval wordPipeline) path
  putStr output
  putStrLn ""
  putStrLn "interval timings:"
  mapM_ (\(name, ts) -> putStrLn $ "  " <> name <> ": " <> fmtMs (sum ts)) $ Map.toList (allLaps ws)

-- ---------------------------------------------------------------------------
-- Function-composition experiment
--
-- Compare timing when the pure word-processing functions are composed
-- (allowing GHC to fuse) versus when they are kept as separate Kleisli
-- stages (forcing the intermediate lists).
-- ---------------------------------------------------------------------------

-- | One line through the separate pure stages, each measured.
--
-- Work is forced only at the end, so most cost collapses into the final
-- 'sep' interval. The intermediate laps measure almost nothing.
separateStages :: FreeTrace (,) (K IO) String ([String], Watches Nanos Nanos)
separateStages =
  start timeX "sep"
    .> carry (K (pure . splitWords))
    .> lap timeX "split"
    .> carry (K (pure . lowerWords))
    .> lap timeX "lower"
    .> carry (K (pure . noEmpties))
    .> carry (K (evaluate . force))
    .> stop timeX "sep"

-- | One line through the separate pure stages, forcing after each stage.
--
-- This defeats fusion and materialises every intermediate list, but it shows
-- where the time actually goes.
separateStagesForced :: FreeTrace (,) (K IO) String ([String], Watches Nanos Nanos)
separateStagesForced =
  start timeX "sep"
    .> carry (K (pure . splitWords))
    .> carry (K (evaluate . force))
    .> lap timeX "split"
    .> carry (K (pure . lowerWords))
    .> carry (K (evaluate . force))
    .> lap timeX "lower"
    .> carry (K (pure . noEmpties))
    .> carry (K (evaluate . force))
    .> lap timeX "filter"
    .> stop timeX "sep"

-- | One line through the fully composed pure function.
fusedStage :: FreeTrace (,) (K IO) String ([String], Watches Nanos Nanos)
fusedStage =
  start timeX "fus"
    .> carry (K (pure . noEmpties . lowerWords . splitWords))
    .> carry (K (evaluate . force))
    .> stop timeX "fus"

-- | Process a repeated line in three ways to expose laziness + fusion effects.
lineExperiment :: String -> IO ()
lineExperiment line = do
  putStrLn "\n--- function-composition experiment ---"
  let wordsPerRun = 100000
      longLine = unwords (replicate wordsPerRun line)
  (_, wsSep) <- runK (eval separateStages) longLine
  (_, wsSepF) <- runK (eval separateStagesForced) longLine
  (_, wsFus) <- runK (eval fusedStage) longLine
  putStrLn $ "input words: " <> show wordsPerRun
  putStrLn "separate (lazy, force at end):"
  mapM_ (\(name, ts) -> putStrLn $ "  " <> name <> ": " <> fmtMs (sum ts)) $ Map.toList (allLaps wsSep)
  putStrLn "separate (force after each stage):"
  mapM_ (\(name, ts) -> putStrLn $ "  " <> name <> ": " <> fmtMs (sum ts)) $ Map.toList (allLaps wsSepF)
  putStrLn "fused (single force):"
  mapM_ (\(name, ts) -> putStrLn $ "  " <> name <> ": " <> fmtMs (sum ts)) $ Map.toList (allLaps wsFus)

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

fmtMs :: Nanos -> String
fmtMs n =
  let ms = fromIntegral n / 1_000_000 :: Double
   in if ms < 0.001 then "<0.001ms" else showFFloat (Just 3) ms "ms"

main :: IO ()
main = do
  wordCount "other/alice.md"
  lineExperiment "The quick brown fox jumps over the lazy dog"
