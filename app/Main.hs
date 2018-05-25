{-# LANGUAGE RankNTypes #-}

module Main where

import Lib

import Criterion.Main

import Data.Array.Accelerate                     as A
import Data.Array.Accelerate.LLVM.Native         as CPU
import Data.Array.Accelerate.LLVM.PTX            as PTX


main :: IO ()
main = defaultMain
  [ bgroup "accelerate"
    [ bgroup "cpu" (benchmarks CPU.run1)
    , bgroup "ptx" (benchmarks PTX.run1)
    ]
  ]
