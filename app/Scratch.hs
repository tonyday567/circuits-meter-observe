{-# LANGUAGE BlockArguments #-}
-- | Scratch: dump Core for Traced Either (->) sum loop vs direct recursion.
module Scratch where

import Circuit.Channel (trace)
import Data.List (foldl')

-- * Either-traced list sum

sumEither :: [Int] -> Int
sumEither xs = trace (either step step) (xs, 0)
  where
    step :: ([Int], Int) -> Either ([Int], Int) Int
    step ([], acc)    = Right acc
    step (x:xs', acc) = Left (xs', x + acc)
{-# NOINLINE sumEither #-}

-- * Direct non-tail recursion (sumCo equivalent)

sumDirect :: [Int] -> Int
sumDirect [] = 0
sumDirect (x:xs) = x + sumDirect xs
{-# NOINLINE sumDirect #-}

-- * Tail-recursive with $!

sumTailRec :: [Int] -> Int
sumTailRec = go 0
  where go acc [] = acc; go acc (x:xs) = go (x + acc) $! xs
{-# NOINLINE sumTailRec #-}

-- * foldl' baseline

sumFoldL :: [Int] -> Int
sumFoldL = foldl' (+) 0
{-# NOINLINE sumFoldL #-}
