


module CNN.HiddenLayer where

import Control.Monad
import System.Random.Mersenne as MT

import CNN.LayerType
import CNN.Image

{- |
initFilterH

  IN: #kernel
      #channel

-}

initFilterH :: Int -> Int -> IO [FilterH]
initFilterH k c = do
  f <- forM [1..k] $ \i -> do
    f' <- initKernel c
    return f'
  return f
  where
    a = 1.0 / fromIntegral c
    initKernel :: Int -> IO FilterH
    initKernel c = do
      w <- forM [1..c] $ \i -> do
        r <- MT.randomIO :: IO Double
        return ((r * 2.0 - 1.0) * a)
      return (0.0:w)

--

{- |
connect

  IN: filter
      image

>>> let fs = [[0.5,1.0,2.0,3.0],[0.1,4.0,5.0,6.0]]
>>> let im = [[[9.0,8.0,7.0]]]
>>> connect fs im
[[[46.5,118.1]]]

-}

connect :: [FilterH] -> Image -> Image
connect [] _ = error "invalid FilterH"
connect _ [] = error "invalid Image"
connect fs [[im]] = [[map (dot im) fs]]
  where
    dot :: [Double] -> [Double] -> Double
    dot im f = sum $ zipWith (*) (1.0:im) f



