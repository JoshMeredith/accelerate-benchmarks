{-# LANGUAGE RankNTypes #-}

module Lib (
    benchmarks
) where

import Criterion.Main
import Data.Array.Accelerate as A


benchmarks
  :: (forall a b. (Arrays a, Arrays b) => (Acc a -> Acc b) -> a -> b)
  -> [Benchmark]
benchmarks run1 =
  [ mapBenchmarks      run1
  , foldBenchmarks     run1
  , stencilBenchmarks  run1
  , generateBenchmarks run1
  ]


bench1D_default :: DIM1
bench1D_default = Z :. 100000000


bench2D_default :: DIM2
bench2D_default = Z :. 5000 :. 5000


bench3D_default :: DIM3
bench3D_default = Z :. 250 :. 250 :. 250


mapBenchmarks
  :: (forall a b. (Arrays a, Arrays b) => (Acc a -> Acc b) -> a -> b)
  -> Benchmark
mapBenchmarks run1 =
  bgroup "mapBenchmarks"
    [ bgroup "" []

    -- 1D
    , accelerateBenchmark run1 "1D/map_plus" (A.map (+1)) bench1D_default

    -- 2D
    , accelerateBenchmark run1 "2D/map_plus" (A.map (+1)) bench2D_default

    -- 3D
    , accelerateBenchmark run1 "3D/map_plus" (A.map (+1)) bench3D_default

    ]


foldBenchmarks
  :: (forall a b. (Arrays a, Arrays b) => (Acc a -> Acc b) -> a -> b)
  -> Benchmark
foldBenchmarks run1 =
  bgroup "foldBenchmarks"
    [ bgroup "" []

    -- 1D
    , accelerateBenchmark run1 "1D/sum" A.sum bench1D_default

    -- 2D
    , accelerateBenchmark run1 "2D/sum" A.sum bench2D_default

    -- 3D
    , accelerateBenchmark run1 "3D/sum" A.sum bench3D_default

    ]


stencilBenchmarks
  :: (forall a b. (Arrays a, Arrays b) => (Acc a -> Acc b) -> a -> b)
  -> Benchmark
stencilBenchmarks run1 =
  bgroup "stencilBenchmarks"
    [
    ]


generateBenchmarks
  :: (forall a b. (Arrays a, Arrays b) => (Acc a -> Acc b) -> a -> b)
  -> Benchmark
generateBenchmarks run1 =
  bgroup "generateBenchmarks"
    [
    ]


benchArray
  :: Shape sh
  => sh
  -> Array sh Int
benchArray size =
  fromList size $ cycle [0..999]


accelerateBenchmark
  :: (Shape sh, Shape sh')
  => (forall a b. (Arrays a, Arrays b) => (Acc a -> Acc b) -> a -> b)
  -> String
  -> (Acc (Array sh Int) -> Acc (Array sh' Int))
  -> sh
  -> Benchmark
accelerateBenchmark run1 name fn size =
  bench name $ whnf (run1 fn) (benchArray size)

