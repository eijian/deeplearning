

module CNN.Kernel where

import Control.Monad
import System.Random.Mersenne as MT

type Kernel = [[Double]]  -- resolution (m x n) x channels(c)
type Bias   = Double
type Filter = [(Kernel, Bias)]


-- n_kernel, channel, reso(ImageSize)
mkFilter :: Int -> Int -> Int -> Double -> IO Filter
mkFilter k c s a = do
  f <- forM [1..k] $ \i -> do
    kn <- forM [1..c] $ \j -> do
      p <- mapM (rnd a) [1..s]
      return p
    return (kn, 0.0)
  return f
  where
    rnd :: Double -> Int -> IO Double
    rnd r i = do
      x <- MT.randomIO :: IO Double
      return ((x * 2.0 - 1.0) * r)
