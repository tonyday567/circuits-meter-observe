{-# LANGUAGE GADTs #-}

-- | Producer-consumer: open, read, close as explicit FreeTrace stages.
module Main where

import Circuit
import Circuit.Category (K (..), (.>))
import Data.Bool (bool)
import System.IO (Handle, IOMode (ReadMode), hClose, hGetLine, hIsEOF, openFile)

-- | Domain primitives #################################################
openf :: FreeTrace t (K IO) FilePath Handle
openf = Lift (K (`openFile` ReadMode))

getLine :: FreeTrace t (K IO) Handle String
getLine = Lift (K hGetLine)

closef :: FreeTrace t (K IO) Handle ()
closef = Lift (K hClose)

-- | Iteration via Either feedback ####################################
readAll :: FreeTrace Either (K IO) Handle (Handle, [String])
readAll = trace (Lift (K step))
  where
    step (Left (h, acc)) =
      hIsEOF h
        >>= bool
          (hGetLine h >>= \line -> pure (Left (h, line : acc)))
          (pure (Right (h, acc)))
    step (Right h) =
      pure (Left (h, []))

-- | Pipeline: open → read → close → display ##########################
pipeline :: FreeTrace Either (K IO) FilePath ()
pipeline =
  openf
    .> readAll
    .> Lift (K (\(h, acc) -> hClose h >> putStrLn ("read " <> show (length acc) <> " lines")))

main :: IO ()
main = runK (eval pipeline) "other/alice.md"
