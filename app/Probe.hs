{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Active probe: run named measurements against golden baselines.
module Main where

import Circuit.Category (Category (..), (.>))
import Circuit.Channel (trace)
import Circuit.Ends (Ends (..), HasUnit (..), In (..), Out (..), close, companion, conjoint, emit)
import Circuit.Hyper (HyperF, pattern Hyper, invoke, lift, liftArr, observe, observeArr, push, pushArr, base, baseArr, runHyper)
import Circuit.Layer (run)
import Circuit.Loop (Loop (..))
import Circuit.Net (enrich, melt)
import Circuit.Process (Process (..), pattern P, scan, fold)
import Circuit.Meter (Meter (..))
import Circuit.Meter.Stopwatch qualified as SW
import Circuit.Meter.Time (Nanos, ticks, timeX)
import Control.Arrow (Kleisli (..), runKleisli)
import Control.DeepSeq (NFData, force)
import Control.Exception (IOException, catch, evaluate)
import Control.Monad (replicateM, replicateM_, void, when)
import Data.IORef
import Data.List (foldl', scanl', sort)
import Data.Map.Strict qualified as Map
import Options.Applicative
import Prelude hiding (id, (.))
import System.IO (Handle, IOMode (ReadMode, WriteMode), hClose, hGetLine, hIsEOF, hPutStrLn, openFile)
import System.IO.Temp (withSystemTempFile)
import System.Exit (exitFailure)
import Text.Printf (printf)

-- ---------------------------------------------------------------------------
-- Config
-- ---------------------------------------------------------------------------

data Mode = ModeSnippet String | ModeList
  deriving (Show, Eq)

data Config = Config
  { cfgMode :: Mode,
    cfgRuns :: Int,
    cfgLengths :: [Int],
    cfgQuantile :: Double,
    cfgRecord :: Bool,
    cfgGolden :: FilePath,
    cfgError :: Double,
    cfgWarn :: Double
  }

parseLengths :: Parser [Int]
parseLengths =
  option
    (eitherReader parseCommaInts)
    ( long "lengths" <> short 'L' <> value [] <> metavar "INT,INT,..."
        <> help "input sizes for scaling check (e.g. 100,200,500,1000)"
    )

parseCommaInts :: String -> Either String [Int]
parseCommaInts s =
  case traverse readMaybe (splitOn ',' s) of
    Just ns -> Right ns
    Nothing -> Left $ "invalid lengths: " ++ s
  where
    splitOn c str = case break (== c) str of
      (a, []) -> [a]
      (a, _ : rest) -> a : splitOn c rest

readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of [(x, "")] -> Just x; _ -> Nothing

parseMode :: Parser Mode
parseMode =
  flag' ModeList (long "list" <> help "list available snippets")
    <|> (ModeSnippet <$> option str (long "snippet" <> short 's' <> metavar "NAME" <> help "snippet to run"))

configP :: Parser Config
configP =
  Config
    <$> parseMode
    <*> option auto (long "runs" <> short 'n' <> value 100 <> metavar "INT" <> help "iterations per length (default 100)")
    <*> (parseLengths <|> (pure <$> option auto (long "length" <> short 'l' <> value 1000 <> metavar "INT" <> help "single input size (default 1000)")))
    <*> option auto (long "quantile" <> short 'q' <> value 0.5 <> metavar "DOUBLE" <> help "quantile (0.5=median, 0.1=p10 clean proxy)")
    <*> switch (long "record" <> short 'r' <> help "write golden file instead of checking")
    <*> option str (long "golden" <> short 'g' <> value "other/circuits-meter.golden" <> metavar "FILE" <> help "golden file path")
    <*> option auto (long "error" <> value 0.2 <> metavar "DOUBLE" <> help "error threshold (default 0.2)")
    <*> option auto (long "warn" <> value 0.05 <> metavar "DOUBLE" <> help "warning threshold (default 0.05)")

-- ---------------------------------------------------------------------------
-- Golden file
-- ---------------------------------------------------------------------------

goldenKey :: String -> Int -> Int -> String
goldenKey name runs len = name <> ":" <> show runs <> ":" <> show len

readGolden :: FilePath -> IO (Map.Map String Nanos)
readGolden path = do
  contents <- readFile path
  let !m = Map.fromList
        [ (k, read v)
        | line <- lines contents,
          not (null line),
          let (k, ' ' : v) = break (== ' ') line
        ]
  evaluate (length m)
  pure m

writeGolden :: FilePath -> Map.Map String Nanos -> IO ()
writeGolden path m = do
  let lines' = [k <> " " <> show v | (k, v) <- Map.toAscList m]
  writeFile path (unlines lines')

readGoldenSafe :: FilePath -> IO (Map.Map String Nanos)
readGoldenSafe path =
  catch (readGolden path) (\(_ :: IOException) -> pure Map.empty)

-- ---------------------------------------------------------------------------
-- Measurement helpers
-- ---------------------------------------------------------------------------

-- | Quantile from a sorted list. q=0.5 → median, q=0.1 → p10.
quantile :: Double -> [Nanos] -> Nanos
quantile q xs =
  let sorted = sort xs
      idx = floor (q * fromIntegral (length sorted - 1))
  in sorted !! max 0 (min idx (length sorted - 1))

-- | Measure a pure function @runs@ times at length @len@, return quantile.
-- The function receives the length as its argument.
measurePure :: (NFData b) => Double -> Int -> (Int -> b) -> Int -> IO Nanos
measurePure q runs f len = do
  (ts, _) <- ticks runs f len
  pure (quantile q ts)

-- | Measure an IO action @runs@ times, return quantile.
measureIO :: Double -> Int -> IO a -> IO Nanos
measureIO q runs action = do
  ts <- replicateM runs $ do
    t0 <- runKleisli (start timeX) ()
    _ <- action
    dt <- runKleisli (stop timeX) t0
    pure dt
  pure (quantile q ts)

-- ---------------------------------------------------------------------------
-- Snippet implementations
-- ---------------------------------------------------------------------------

-- Each snippet is @Int -> IO Nanos@ where the Int is the length.

idApply :: Int -> Int
idApply = id
{-# NOINLINE idApply #-}

closureApply :: Int -> Int
closureApply n = let f x = x + 1 in f n
{-# NOINLINE closureApply #-}

buildList :: Int -> [Int]
buildList n = go n []
  where go 0 acc = acc; go i acc = go (i - 1) (i : acc)
{-# NOINLINE buildList #-}

sumList :: [Int] -> Int
sumList = foldl' (+) 0
{-# NOINLINE sumList #-}

foldSum :: Int -> Int
foldSum n = go 0 n
  where go acc 0 = acc; go acc i = go (acc + i) (i - 1)
{-# NOINLINE foldSum #-}

appendList :: [Int] -> [Int]
appendList xs = xs ++ xs
{-# NOINLINE appendList #-}

arithLoop :: Int -> Int
arithLoop n = go 1 n
  where go acc 0 = acc; go acc i = go (acc * i) (i - 1)
{-# NOINLINE arithLoop #-}

nub' :: [Int] -> [Int]
nub' [] = []
nub' (x : xs) = x : nub' (filter (/= x) xs)
{-# NOINLINE nub' #-}

countIORef :: Int -> IO Int
countIORef target = do
  ref <- newIORef (0 :: Int)
  let loop = do
        n <- readIORef ref
        if n >= target then pure n
        else writeIORef ref (n + 1) >> loop
  loop
{-# NOINLINE countIORef #-}

runPipelineStopwatch :: Int -> IO (Int, SW.Watches Nanos Nanos)
runPipelineStopwatch n = runKleisli (run pipeline) n
  where
    pipeline =
      SW.start timeX "total"
        .> SW.carry (Kleisli (\x -> evaluate (force (sum [0 .. x]))))
        .> SW.lap timeX "sum"
        .> SW.stop timeX "total"

-- Snippet wrappers

snippetId :: Int -> Config -> IO Nanos
snippetId len cfg = measurePure (cfgQuantile cfg) (cfgRuns cfg) idApply len

snippetClosure :: Int -> Config -> IO Nanos
snippetClosure len cfg = measurePure (cfgQuantile cfg) (cfgRuns cfg) closureApply len

snippetCons :: Int -> Config -> IO Nanos
snippetCons len cfg = measurePure (cfgQuantile cfg) (cfgRuns cfg) (length . buildList) len

snippetSum :: Int -> Config -> IO Nanos
snippetSum len cfg = measurePure (cfgQuantile cfg) (cfgRuns cfg) (sumList . buildList) len

snippetFold :: Int -> Config -> IO Nanos
snippetFold len cfg = measurePure (cfgQuantile cfg) (cfgRuns cfg) foldSum len

snippetAppend :: Int -> Config -> IO Nanos
snippetAppend len cfg = measurePure (cfgQuantile cfg) (cfgRuns cfg) (length . appendList . buildList) len

snippetArith :: Int -> Config -> IO Nanos
snippetArith len cfg = measurePure (cfgQuantile cfg) (cfgRuns cfg) arithLoop len

snippetNub :: Int -> Config -> IO Nanos
snippetNub len cfg = measurePure (cfgQuantile cfg) (cfgRuns cfg) (\n -> length (nub' [1..n])) len

snippetLoop :: Int -> Config -> IO Nanos
snippetLoop len cfg = measureIO (cfgQuantile cfg) (cfgRuns cfg) (countIORef len)

snippetStopwatch :: Int -> Config -> IO Nanos
snippetStopwatch len cfg = measureIO (cfgQuantile cfg) (cfgRuns cfg) (void (runPipelineStopwatch len))

snippetMeterAction :: Int -> Config -> IO Nanos
snippetMeterAction len cfg = measurePure (cfgQuantile cfg) (cfgRuns cfg) (\n -> foldSum n) len

snippetBracket :: Int -> Config -> IO Nanos
snippetBracket _len cfg = measureIO (cfgQuantile cfg) (cfgRuns cfg) (pure ())

-- ---------------------------------------------------------------------------
-- Recursion patterns (from Perf.Algos v0.12)
-- ---------------------------------------------------------------------------

-- * Sum variants

sumTail :: (Num a) => [a] -> a
sumTail = go 0
  where go acc [] = acc; go acc (x : xs) = go (x + acc) $! xs
{-# NOINLINE sumTail #-}

sumTailLazy :: (Num a) => [a] -> a
sumTailLazy = go 0
  where go acc [] = acc; go acc (x : xs) = go (x + acc) xs
{-# NOINLINE sumTailLazy #-}

sumCo :: (Num a) => [a] -> a
sumCo [] = 0
sumCo (x : xs) = x + sumCo xs
{-# NOINLINE sumCo #-}

sumFoldr :: (Num a) => [a] -> a
sumFoldr = foldr (+) 0
{-# NOINLINE sumFoldr #-}

sumMono :: [Int] -> Int
sumMono = foldl' (+) 0
{-# NOINLINE sumMono #-}

sumPoly :: (Num a) => [a] -> a
sumPoly = foldl' (+) 0
{-# NOINLINE sumPoly #-}

sumSum :: (Num a) => [a] -> a
sumSum = sum
{-# NOINLINE sumSum #-}

-- * Length variants

lengthTail :: [a] -> Int
lengthTail = go 0
  where go n [] = n; go n (_ : xs) = go (n + 1) $! xs
{-# NOINLINE lengthTail #-}

lengthFoldr :: [a] -> Int
lengthFoldr = foldr (\_ n -> n + 1) 0
{-# NOINLINE lengthFoldr #-}

lengthPrelude :: [a] -> Int
lengthPrelude = length
{-# NOINLINE lengthPrelude #-}

-- * Recursion (generic, no list)

recurseTail :: Int -> Int
recurseTail n = go 0 n
  where go acc 0 = acc; go acc i = go (acc + 1) $! (i - 1)
{-# NOINLINE recurseTail #-}

recurseCo :: Int -> Int
recurseCo 0 = 0
recurseCo n = 1 + recurseCo (n - 1)
{-# NOINLINE recurseCo #-}

-- * Tree traversal — stack vs heap continuation

data Tree = Leaf Int | Node Tree Tree
  deriving (Show)

-- | Build left-spine tree of given depth (2n+1 nodes).
buildTree :: Int -> Tree
buildTree 0 = Leaf 1
buildTree n = Node (buildTree (n - 1)) (Leaf 1)
{-# NOINLINE buildTree #-}

-- | Direct non-tail recursion — builds O(depth) thunks, GC pressure at depth.
sumTreeDirect :: Tree -> Int
sumTreeDirect (Leaf n) = n
sumTreeDirect (Node l r) = sumTreeDirect l + sumTreeDirect r
{-# NOINLINE sumTreeDirect #-}

-- | Tail-recursive with explicit heap stack — constant stack, O(depth) heap.
sumTreeStack :: Tree -> Int
sumTreeStack t = go t [] 0
  where
    go (Leaf n) stack acc = case stack of
      [] -> acc + n
      (t' : stack') -> go t' stack' (acc + n)
    go (Node l r) stack acc = go l (r : stack) acc
{-# NOINLINE sumTreeStack #-}

-- | Either-traced — delimited continuation on the heap via 'trace'.
-- The continuation is the 'go' closure in 'Traced Either (->)' — @go (Right state)@
-- with @Left@ feeding back, @Right@ returning. The computation lives on the heap
-- as a closure rather than on the GHC eval stack as nested thunks.
sumTreeEither :: Tree -> Int
sumTreeEither t = trace (either step step) (t, [], 0)
  where
    step :: (Tree, [Tree], Int) -> Either (Tree, [Tree], Int) Int
    step (Leaf n, stack, acc) = case stack of
      [] -> Right (acc + n)
      (t' : stack') -> Left (t', stack', acc + n)
    step (Node l r, stack, acc) = Left (l, r : stack, acc)
{-# NOINLINE sumTreeEither #-}

-- * Hyper — timing the hyperfunction encoding

-- | Lift a function into Hyper and observe it back — cost per round-trip.
-- Calls @observe (lift (+1))@ N times.
hyperRoundTrip :: Int -> Int
hyperRoundTrip n = go n 0
  where go 0 acc = acc; go i acc = go (i - 1) (observe (lift (+1)) acc)
{-# NOINLINE hyperRoundTrip #-}

-- | Push N functions onto a base Hyper, then observe.
-- Builds a chain: push f . push g . push h ... . base k
hyperPushChain :: Int -> Int
hyperPushChain n = observe (go n (base 0)) n
  where
    go 0 h = push (+1) h
    go i h = go (i - 1) (push (+1) h)
{-# NOINLINE hyperPushChain #-}

-- | Compose N hyperfunctions with (.).
-- Each composition builds a new Hyper wrapping the continuation.
hyperCompose :: Int -> Int
hyperCompose n = observe (go n) 0
  where
    go 0 = lift (+1)
    go i = go (i - 1) . lift (+1)
{-# NOINLINE hyperCompose #-}

-- | Build a Hyper via the final encoding and run it through 'invoke' N times.
-- Each step: @invoke (go i) (go (i-1))@ — feeds one Hyper as continuation to the next.
-- Measures traversal cost of the nested closure chain without lift/push.
hyperNest :: Int -> Int
hyperNest n = go n (lift (+1))
  where
    go 0 h = observe h 0
    go i h = go (i - 1) (Hyper $ \k -> invoke h k)
{-# NOINLINE hyperNest #-}

-- | runHyper on a self-referential loop — the lazy knot overhead.
-- Creates and runs a Hyper knot N times.
hyperRun :: Int -> Int
hyperRun n = go n 0
  where
    go 0 acc = acc
    go i acc = go (i - 1) (runHyper (Hyper $ \k -> invoke k (Hyper (const (acc + 1)))))
{-# NOINLINE hyperRun #-}

-- | Direct function composition for comparison to Hyper composition.
directCompose :: Int -> Int
directCompose n = go n 0
  where
    go 0 x = x + 1
    go i x = go (i - 1) (x + 1)
{-# NOINLINE directCompose #-}

-- * Traced (,) — lazy knot

-- | Traced (,) trace: the lazy knot. Produces an infinite list via feedback;
-- returns the nth element. The knot ties @ns = 0 : map (+1) ns@.
traceLazyKnot :: Int -> Int
traceLazyKnot n = head $ drop n (trace step ())
  where
    step :: ([Int], ()) -> ([Int], [Int])
    step (ns, ()) = (0 : map (+1) ns, ns)
{-# NOINLINE traceLazyKnot #-}

-- | Same lazy list without the trace — direct lazy knot for comparison.
directKnot :: Int -> Int
directKnot n = head $ drop n ns
  where
    ~(ns, _) = (0 : map (+1) ns, ())
{-# NOINLINE directKnot #-}

-- * Loop — free traced monoidal category

-- | Lift a plain function into Loop and run it back — round-trip cost.
loopLift :: Int -> Int
loopLift n = go n 0
  where
    go 0 acc = acc
    go i acc = go (i - 1) (run (Lift (+1) :: Loop (,) (->) Int Int) acc)
{-# NOINLINE loopLift #-}

-- | Knot a feedback loop in Loop and run it — the Knot constructor cost.
-- Uses a productive lazy-list knot like traceLazyKnot.
loopKnot :: Int -> Int
loopKnot n = head $ drop n (run knot ())
  where
    step :: ([Int], ()) -> ([Int], [Int])
    step (ns, ()) = (0 : map (+1) ns, ns)
    knot :: Loop (,) (->) () [Int]
    knot = Knot step
{-# NOINLINE loopKnot #-}

-- | Compose N Loop Lifts with Category (.) and run.
loopCompose :: Int -> Int
loopCompose n = run (go n :: Loop (,) (->) Int Int) 0
  where
    go :: Int -> Loop (,) (->) Int Int
    go 0 = Lift (+1)
    go i = go (i - 1) . Lift (+1)
{-# NOINLINE loopCompose #-}

-- * Net — free traced PROP with bimonoid

-- | Enrich a Loop into Net and melt it back — round-trip cost.
netRoundTrip :: Int -> Int
netRoundTrip n = go n 0
  where
    go 0 acc = acc
    go i acc = go (i - 1) (run (melt (enrich (Lift (+1) :: Loop (,) (->) Int Int)) :: Loop (,) (->) Int Int) acc)
{-# NOINLINE netRoundTrip #-}

-- * Layer — free construction interface

-- | Compose N morphisms with .> (Layer composition) and run.
layerCompose :: Int -> Int
layerCompose n = run (go n :: Loop (,) (->) Int Int) 0
  where
    go :: Int -> Loop (,) (->) Int Int
    go 0 = Lift (+1)
    go i = Lift (+1) .> go (i - 1)
{-# NOINLINE layerCompose #-}

-- * Process — Moore machine benchmarks

-- | Process sum: P id (+) id — running sum via Moore machine.
-- Compare to scanl'.
processScan :: [Int] -> [Int]
processScan = scan (P id (+) id)
{-# NOINLINE processScan #-}

-- | Process fold: final value of processScan.
processFold :: [Int] -> Maybe Int
processFold = fold (P id (+) id)
{-# NOINLINE processFold #-}

-- | Direct scanl' for comparison.
directScan :: [Int] -> [Int]
directScan = scanl' (+) 0
{-# NOINLINE directScan #-}

-- | Direct foldl' for comparison.
directFold :: [Int] -> Int
directFold = foldl' (+) 0
{-# NOINLINE directFold #-}

-- * Ends — channel end benchmarks

-- | Open a unit Ends pair — the allocation cost.
endsOpen :: Int -> Int
endsOpen n = go n 0
  where
    go 0 acc = acc
    go i acc = let _ = open :: Ends (->) () () in go (i - 1) (acc + 1)
{-# NOINLINE endsOpen #-}

-- | Close an Ends pair — the round-trip cost.
endsClose :: Int -> Int
endsClose n = go n 0
  where
    e = open :: Ends (->) () ()
    go 0 acc = acc
    go i acc = close (conjoint e) (companion e) () `seq` go (i - 1) (acc + 1)
{-# NOINLINE endsClose #-}

-- | One-sided emit cost.
endsEmit :: Int -> Int
endsEmit n = go n 0
  where
    e = open :: Ends (->) () ()
    go 0 acc = acc
    go i acc = emit (companion e) (conjoint e) () `seq` go (i - 1) (acc + 1)
{-# NOINLINE endsEmit #-}

-- | One-sided commit cost.
endsCommit :: Int -> Int
endsCommit n = go n 0
  where
    e = open :: Ends (->) () ()
    go 0 acc = acc
    go i acc = commit (conjoint e) (companion e) () `seq` go (i - 1) (acc + 1)
{-# NOINLINE endsCommit #-}

-- * Words — resource acquisition and Either loop

-- | Write N lines to a temp file, read them back using Loop Either (Kleisli IO),
-- returning the line count. Measures resource acquisition + effectful loop.
wordsLoop :: Int -> IO Int
wordsLoop n = withSystemTempFile "words-bench" $ \fp h -> do
  -- write N lines
  replicateM_ n (hPutStrLn h (show (n :: Int)))
  hClose h
  -- read them back using the Loop Either pattern
  (_, acc) <- runKleisli (run pipeline) fp
  pure (length acc)
  where
    openf :: Loop t (Kleisli IO) FilePath Handle
    openf = Lift (Kleisli (flip openFile ReadMode))

    readAll :: Loop Either (Kleisli IO) Handle (Handle, [String])
    readAll = Knot (Kleisli step)
      where
        step (Left (h, acc)) = do
          done <- hIsEOF h
          if done
            then pure (Right (h, acc))
            else do
              line <- hGetLine h
              pure (Left (h, line : acc))
        step (Right h) = pure (Left (h, []))

    pipeline :: Loop Either (Kleisli IO) FilePath (Handle, [String])
    pipeline = openf .> readAll .> Lift (Kleisli (\(h, acc) -> hClose h >> pure (h, acc)))
{-# NOINLINE wordsLoop #-}

-- | Direct IO read for comparison — open, read, close without Loop.
wordsDirect :: Int -> IO Int
wordsDirect n = withSystemTempFile "words-direct" $ \fp h -> do
  replicateM_ n (hPutStrLn h (show (n :: Int)))
  hClose h
  h2 <- openFile fp ReadMode
  -- read all lines
  let go acc = do
        done <- hIsEOF h2
        if done then pure acc
        else do line <- hGetLine h2; go (line : acc)
  acc <- go []
  hClose h2
  pure (length acc)
{-# NOINLINE wordsDirect #-}

-- * Kleisli IO variants — monadic overhead

-- | Loop Lift + run in Kleisli IO. Compare to loopLift (pure).
kLoopLift :: Int -> IO Int
kLoopLift n = go n 0
  where
    go 0 acc = pure acc
    go i acc = runKleisli (run (Lift (Kleisli (pure . (+1))) :: Loop (,) (Kleisli IO) Int Int)) acc >>= go (i - 1)
{-# NOINLINE kLoopLift #-}

-- | Loop Knot + run in Kleisli IO (Either trace). Compare to loopKnot.
kLoopKnot :: Int -> IO Int
kLoopKnot n = go n 0
  where
    step :: Either (Int, ()) () -> IO (Either (Int, ()) Int)
    step (Right ()) = pure (Left (0, ()))
    step (Left (acc, ())) = pure (Right (acc + 1))
    knot = Knot (Kleisli step) :: Loop Either (Kleisli IO) () Int
    go 0 acc = pure acc
    go i acc = runKleisli (run knot) () >>= go (i - 1)
{-# NOINLINE kLoopKnot #-}

-- | Hyper push chain in Kleisli IO. Compare to hyperPushChain.
kHyperPush :: Int -> IO Int
kHyperPush n = do
  let h = go n (baseArr (0 :: Int)) :: HyperF (Kleisli IO) Int Int
  observeArr h n
  where
    go 0 h = pushArr (Kleisli (pure . (+1))) h
    go i h = go (i - 1) (pushArr (Kleisli (pure . (+1))) h)
{-# NOINLINE kHyperPush #-}

-- | Hyper compose in Kleisli IO. Compare to hyperCompose.
kHyperCompose :: Int -> IO Int
kHyperCompose n = do
  let h = go n :: HyperF (Kleisli IO) Int Int
  observeArr h 0
  where
    go 0 = liftArr (Kleisli (pure . (+1)))
    go i = go (i - 1) . liftArr (Kleisli (pure . (+1)))
{-# NOINLINE kHyperCompose #-}

-- | Direct Kleisli loop — manual bind, no circuits machinery.
kDirect :: Int -> IO Int
kDirect n = go n 0
  where
    step = pure . (+1)
    go 0 acc = pure acc
    go i acc = step acc >>= go (i - 1)
{-# NOINLINE kDirect #-}

snippetSumTail :: Int -> Config -> IO Nanos
snippetSumTail len cfg = measurePure (cfgQuantile cfg) (cfgRuns cfg) (sumTail . buildList) len

snippetSumTailLazy :: Int -> Config -> IO Nanos
snippetSumTailLazy len cfg = measurePure (cfgQuantile cfg) (cfgRuns cfg) (sumTailLazy . buildList) len

snippetSumCo :: Int -> Config -> IO Nanos
snippetSumCo len cfg = measurePure (cfgQuantile cfg) (cfgRuns cfg) (sumCo . buildList) len

snippetSumFoldr :: Int -> Config -> IO Nanos
snippetSumFoldr len cfg = measurePure (cfgQuantile cfg) (cfgRuns cfg) (sumFoldr . buildList) len

snippetSumMono :: Int -> Config -> IO Nanos
snippetSumMono len cfg = measurePure (cfgQuantile cfg) (cfgRuns cfg) (sumMono . buildList) len

snippetSumPoly :: Int -> Config -> IO Nanos
snippetSumPoly len cfg = measurePure (cfgQuantile cfg) (cfgRuns cfg) (sumPoly . buildList) len

snippetSumSum :: Int -> Config -> IO Nanos
snippetSumSum len cfg = measurePure (cfgQuantile cfg) (cfgRuns cfg) (sumSum . buildList) len

snippetLengthTail :: Int -> Config -> IO Nanos
snippetLengthTail len cfg = measurePure (cfgQuantile cfg) (cfgRuns cfg) (lengthTail . buildList) len

snippetLengthFoldr :: Int -> Config -> IO Nanos
snippetLengthFoldr len cfg = measurePure (cfgQuantile cfg) (cfgRuns cfg) (lengthFoldr . buildList) len

snippetLengthPrelude :: Int -> Config -> IO Nanos
snippetLengthPrelude len cfg = measurePure (cfgQuantile cfg) (cfgRuns cfg) (lengthPrelude . buildList) len

snippetRecurseTail :: Int -> Config -> IO Nanos
snippetRecurseTail len cfg = measurePure (cfgQuantile cfg) (cfgRuns cfg) recurseTail len

snippetRecurseCo :: Int -> Config -> IO Nanos
snippetRecurseCo len cfg = measurePure (cfgQuantile cfg) (cfgRuns cfg) recurseCo len

snippetSumTreeDirect :: Int -> Config -> IO Nanos
snippetSumTreeDirect len cfg = measurePure (cfgQuantile cfg) (cfgRuns cfg) (sumTreeDirect . buildTree) len

snippetSumTreeStack :: Int -> Config -> IO Nanos
snippetSumTreeStack len cfg = measurePure (cfgQuantile cfg) (cfgRuns cfg) (sumTreeStack . buildTree) len

snippetSumTreeEither :: Int -> Config -> IO Nanos
snippetSumTreeEither len cfg = measurePure (cfgQuantile cfg) (cfgRuns cfg) (sumTreeEither . buildTree) len

-- Hyper snippets

snippetHyperRoundTrip :: Int -> Config -> IO Nanos
snippetHyperRoundTrip len cfg = measurePure (cfgQuantile cfg) (cfgRuns cfg) hyperRoundTrip len

snippetHyperPushChain :: Int -> Config -> IO Nanos
snippetHyperPushChain len cfg = measurePure (cfgQuantile cfg) (cfgRuns cfg) hyperPushChain len

snippetHyperCompose :: Int -> Config -> IO Nanos
snippetHyperCompose len cfg = measurePure (cfgQuantile cfg) (cfgRuns cfg) hyperCompose len

snippetHyperNest :: Int -> Config -> IO Nanos
snippetHyperNest len cfg = measurePure (cfgQuantile cfg) (cfgRuns cfg) hyperNest len

snippetHyperRun :: Int -> Config -> IO Nanos
snippetHyperRun len cfg = measurePure (cfgQuantile cfg) (cfgRuns cfg) hyperRun len

snippetDirectCompose :: Int -> Config -> IO Nanos
snippetDirectCompose len cfg = measurePure (cfgQuantile cfg) (cfgRuns cfg) directCompose len

-- Circuits component snippets

snippetTraceLazyKnot :: Int -> Config -> IO Nanos
snippetTraceLazyKnot len cfg = measurePure (cfgQuantile cfg) (cfgRuns cfg) traceLazyKnot len

snippetDirectKnot :: Int -> Config -> IO Nanos
snippetDirectKnot len cfg = measurePure (cfgQuantile cfg) (cfgRuns cfg) directKnot len

snippetLoopLift :: Int -> Config -> IO Nanos
snippetLoopLift len cfg = measurePure (cfgQuantile cfg) (cfgRuns cfg) loopLift len

snippetLoopKnot :: Int -> Config -> IO Nanos
snippetLoopKnot len cfg = measurePure (cfgQuantile cfg) (cfgRuns cfg) loopKnot len

snippetLoopCompose :: Int -> Config -> IO Nanos
snippetLoopCompose len cfg = measurePure (cfgQuantile cfg) (cfgRuns cfg) loopCompose len

snippetNetRoundTrip :: Int -> Config -> IO Nanos
snippetNetRoundTrip len cfg = measurePure (cfgQuantile cfg) (cfgRuns cfg) netRoundTrip len

snippetLayerCompose :: Int -> Config -> IO Nanos
snippetLayerCompose len cfg = measurePure (cfgQuantile cfg) (cfgRuns cfg) layerCompose len

-- Process snippets

snippetProcessScan :: Int -> Config -> IO Nanos
snippetProcessScan len cfg = measurePure (cfgQuantile cfg) (cfgRuns cfg) (last . processScan . buildList) len

snippetProcessFold :: Int -> Config -> IO Nanos
snippetProcessFold len cfg = measurePure (cfgQuantile cfg) (cfgRuns cfg) (maybe 0 id . processFold . buildList) len

snippetDirectScan :: Int -> Config -> IO Nanos
snippetDirectScan len cfg = measurePure (cfgQuantile cfg) (cfgRuns cfg) (last . directScan . buildList) len

snippetDirectFold :: Int -> Config -> IO Nanos
snippetDirectFold len cfg = measurePure (cfgQuantile cfg) (cfgRuns cfg) (directFold . buildList) len

-- Ends snippets

snippetEndsOpen :: Int -> Config -> IO Nanos
snippetEndsOpen len cfg = measurePure (cfgQuantile cfg) (cfgRuns cfg) endsOpen len

snippetEndsClose :: Int -> Config -> IO Nanos
snippetEndsClose len cfg = measurePure (cfgQuantile cfg) (cfgRuns cfg) endsClose len

snippetEndsEmit :: Int -> Config -> IO Nanos
snippetEndsEmit len cfg = measurePure (cfgQuantile cfg) (cfgRuns cfg) endsEmit len

snippetEndsCommit :: Int -> Config -> IO Nanos
snippetEndsCommit len cfg = measurePure (cfgQuantile cfg) (cfgRuns cfg) endsCommit len

-- Words IO snippets (use measureIO for effectful benchmarks)

snippetWordsLoop :: Int -> Config -> IO Nanos
snippetWordsLoop len cfg = measureIO (cfgQuantile cfg) (cfgRuns cfg) (wordsLoop len)

snippetWordsDirect :: Int -> Config -> IO Nanos
snippetWordsDirect len cfg = measureIO (cfgQuantile cfg) (cfgRuns cfg) (wordsDirect len)

-- Kleisli IO snippets

snippetKLoopLift :: Int -> Config -> IO Nanos
snippetKLoopLift len cfg = measureIO (cfgQuantile cfg) (cfgRuns cfg) (kLoopLift len)

snippetKLoopKnot :: Int -> Config -> IO Nanos
snippetKLoopKnot len cfg = measureIO (cfgQuantile cfg) (cfgRuns cfg) (kLoopKnot len)

snippetKHyperPush :: Int -> Config -> IO Nanos
snippetKHyperPush len cfg = measureIO (cfgQuantile cfg) (cfgRuns cfg) (kHyperPush len)

snippetKHyperCompose :: Int -> Config -> IO Nanos
snippetKHyperCompose len cfg = measureIO (cfgQuantile cfg) (cfgRuns cfg) (kHyperCompose len)

snippetKDirect :: Int -> Config -> IO Nanos
snippetKDirect len cfg = measureIO (cfgQuantile cfg) (cfgRuns cfg) (kDirect len)

-- ---------------------------------------------------------------------------
-- Snippet registry
-- ---------------------------------------------------------------------------

data Snippet = Snippet
  { snName :: String,
    snDesc :: String,
    snMeasure :: Int -> Config -> IO Nanos
  }

snippets :: [Snippet]
snippets =
  [ -- ground floor
    Snippet "id" "function application (id)" snippetId,
    Snippet "closure" "closure creation + application" snippetClosure,
    Snippet "cons" "list construction with (:)" snippetCons,
    Snippet "sum" "strict left fold over list" snippetSum,
    Snippet "fold" "foldl' in one pass, no intermediate list" snippetFold,
    Snippet "append" "list append (++)" snippetAppend,
    Snippet "arith" "tight arithmetic loop" snippetArith,
    -- substrate
    Snippet "nub" "nub [1..n] — O(n²) reference" snippetNub,
    Snippet "loop" "IORef counting loop" snippetLoop,
    Snippet "stopwatch" "stopwatch pipeline overhead" snippetStopwatch,
    Snippet "meter-action" "meterAction bracket overhead" snippetMeterAction,
    Snippet "bracket" "raw meterAction overhead (empty body)" snippetBracket,
    -- recursion patterns (from Perf.Algos)
    Snippet "sumTail" "sum — tail recursive (strict)" snippetSumTail,
    Snippet "sumTailLazy" "sum — tail recursive (lazy accumulator, space leak)" snippetSumTailLazy,
    Snippet "sumCo" "sum — co-routine (non-tail, builds stack)" snippetSumCo,
    Snippet "sumFoldr" "sum — foldr-based" snippetSumFoldr,
    Snippet "sumMono" "sum — monomorphic (Int, foldl')" snippetSumMono,
    Snippet "sumPoly" "sum — polymorphic (Num a, foldl')" snippetSumPoly,
    Snippet "sumSum" "sum — Prelude.sum" snippetSumSum,
    Snippet "lengthTail" "length — tail recursive" snippetLengthTail,
    Snippet "lengthFoldr" "length — foldr-based" snippetLengthFoldr,
    Snippet "lengthPrelude" "length — Prelude.length" snippetLengthPrelude,
    Snippet "recurseTail" "recurse — tail recursive (no list)" snippetRecurseTail,
    Snippet "recurseCo" "recurse — co-routine (no list)" snippetRecurseCo,
    -- tree traversal — stack vs heap continuation
    Snippet "sumTreeDirect" "tree sum — non-tail (stack thunks, GC pressure)" snippetSumTreeDirect,
    Snippet "sumTreeStack" "tree sum — explicit heap stack (tail rec)" snippetSumTreeStack,
    Snippet "sumTreeEither" "tree sum — Either trace (delimited continuation)" snippetSumTreeEither
  ]

-- ---------------------------------------------------------------------------
-- Scaling summary
-- ---------------------------------------------------------------------------

scalingSummary :: [(Int, Nanos)] -> String
scalingSummary [] = ""
scalingSummary pts =
  let nsPerElem = [(l, fromIntegral t / fromIntegral l :: Double) | (l, t) <- pts, l > 0]
      vals = map snd nsPerElem
      mn = minimum vals
      mx = maximum vals
      spread = if mn > 0 then (mx - mn) / mn else 0
      avg = sum vals / fromIntegral (length vals)
  in if length vals < 2
    then ""
    else printf "  ns/elem: %.2f–%.2f (avg %.2f, spread %.0f%%)" mn mx avg (spread * 100)

-- ---------------------------------------------------------------------------
-- Run
-- ---------------------------------------------------------------------------

runCheck :: Config -> Snippet -> IO ()
runCheck cfg sn = do
  let runs = cfgRuns cfg
      lens = cfgLengths cfg
      modeName = snName sn

  if cfgRecord cfg
    then putStrLn $ "recording: " ++ modeName ++ " (runs=" ++ show runs ++ ")"
    else putStrLn $ "checking: " ++ modeName ++ " (runs=" ++ show runs ++ ")"

  printf "  %-8s %10s\n" ("length" :: String) ("time" :: String)
  printf "  %-8s %10s\n" ("------" :: String) ("----" :: String)

  results <- mapM (\len -> do
    let key = goldenKey modeName runs len
    actual <- snMeasure sn len cfg
    if cfgRecord cfg
      then do
        existing <- readGoldenSafe (cfgGolden cfg)
        let updated = Map.insert key actual existing
        writeGolden (cfgGolden cfg) updated
        printf "  %-8d %7s ns  (recorded)\n" len (show actual)
        pure (len, actual)
      else do
        existing <- readGolden (cfgGolden cfg)
        case Map.lookup key existing of
          Nothing -> do
            printf "  %-8d %7s ns  no golden\n" len (show actual)
            pure (len, actual)
          Just expected -> do
            let ratio = fromIntegral actual / fromIntegral expected :: Double
                change = (ratio - 1) * 100
                sign = if change >= 0 then "+" else ""
                status
                  | ratio > 1 + cfgError cfg = "ERROR"
                  | ratio > 1 + cfgWarn cfg = "WARNING"
                  | otherwise = "OK"
            printf "  %-8d %7s ns  (golden %7s ns, %s%d%%) %s\n" len (show actual) (show expected) sign (round change :: Int) status
            pure (len, actual)
    ) lens

  when (length lens > 1) $ do
    putStrLn ""
    putStrLn $ scalingSummary results

  putStrLn ""

-- ---------------------------------------------------------------------------
-- Main
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  cfg <- execParser (info (configP <**> helper) (fullDesc <> header "circuits-meter probe"))

  when (null (cfgLengths cfg)) $ do
    putStrLn "error: no length specified (use --length or --lengths)"
    exitFailure

  case cfgMode cfg of
    ModeList -> do
      putStrLn "available snippets:"
      mapM_ (\sn -> printf "  %-15s — %s\n" (snName sn) (snDesc sn)) snippets
    ModeSnippet name -> case filter ((== name) . snName) snippets of
      [] -> putStrLn $ "snippet not found: " ++ name ++ "\nuse --list to see available snippets"
      (sn : _) -> runCheck cfg sn
