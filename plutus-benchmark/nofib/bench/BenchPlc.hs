{- | Plutus benchmarks based on some nofib examples. -}
module Main where

import PlutusBenchmark.Common (benchTermCek)
import Shared (benchWith)

main :: IO ()
main = benchWith benchTermCek
