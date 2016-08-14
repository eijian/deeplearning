--
-- ConvLayer : convolution layer
--

module CNN.ConvLayer (
  initFilterC
, convolve
) where

import Debug.Trace
import Control.Monad
import Data.List
import qualified Data.Map as M
import Data.Maybe
import System.Random.Mersenne as MT

import CNN.LayerType
import CNN.Image

{- |
  initFilterC

  IN: #kernel
      #channel
      input size (x * y)
      kernel size (n * n)
      pool size (m * m)
-}

initFilterC :: Int -> Int -> Int -> Int -> Int -> Int -> IO [FilterC]
initFilterC k c x y n m = do
  f <- forM [1..k] $ \i -> do
    w <- initKernel c n
    return (w, 0.0)
  return f
  where
    ri = x * y * c   -- resolution (input)
    ro = (x - n + 1) * (y - n + 1) * k `div` (m * m)
    a = sqrt (6.0 / fromIntegral (ri + ro))
    initKernel :: Int -> Int -> IO Kernel
    initKernel c n = do
      let sz = n * n
      w <- forM [1..c] $ \i -> do
        w' <- forM [1..sz] $ \j -> do
          r <- MT.randomIO :: IO Double
          return ((r * 2.0 - 1.0) * a)
        return w'
      return w

--

{- |
convolve

  IN: kernel size
      filter
      image

test: filter k=3, c=2, s=2
>>> let filter = [([[1.0,2.0,3.0,4.0],[5.0,6.0,7.0,8.0]],0.25),([[3.0,4.0,5.0,6.0],[7.0,8.0,1.0,2.0]],0.5),([[5.0,6.0,7.0,8.0],[1.0,2.0,3.0,4.0]],0.75)] :: [FilterC]
>>> let img1 = [[[1.0,2.0,3.0],[4.0,5.0,6.0],[7.0,8.0,9.0]],[[4.0,5.0,6.0],[7.0,8.0,9.0],[1.0,2.0,3.0]]] :: Image
>>> let img2 = [[[1.0,2.0,3.0,4.0],[4.0,5.0,6.0,7.0],[7.0,8.0,9.0,10.0],[10.0,11.0,12.0,13.0]],[[4.0,5.0,6.0,7.0],[7.0,8.0,9.0,10.0],[1.0,2.0,3.0,4.0],[4.0,5.0,6.0,7.0]]] :: Image
>>> convolve 2 filter img2
[]
>>> convolve 2 filter img1
[[[200.25,236.25],[173.25,209.25]],[[152.5,188.5],[233.5,269.5]],[[152.75,188.75],[197.75,233.75]]]

-}

convolve :: Int -> [FilterC] -> Image -> Image
convolve s fs im = map (convolveImage s im) fs

convolveImage :: Int -> Image -> FilterC -> Plain
convolveImage s im (ks, b) = sumPlains b (zipWith (convolvePlain s) ks im)

sumPlains :: Double -> [Plain] -> Plain
sumPlains b [] = repeat $ repeat b  -- 2 Dimensions (X*Y)
sumPlains b (p:ps) = zipWith f p (sumPlains b ps)
  where
    f :: [Double] -> [Double] -> [Double]
    f [] _          = []
    f (a:[]) (b:_) = [a + b]
    f (a:as) (b:bs) = (a + b):(f as bs)

{- |
convolvePlain

  IN: kernel size (size * size)
      image plain (each channel)
      kernel data

>>> let p = [[1.0,2.0,3.0,4.0],[5.0,6.0,7.0,8.0],[1.0,3.0,5.0,7.0],[2.0,4.0,6.0,8.0]]
>>> let k = [0.25,0.5,0.75,1.0]
>>> convolvePlain 2 k p
[[11.0,13.5,16.0],[8.0,12.25,16.5],[7.25,12.25,17.25]]

>>> let p = [[1.0,2.0,3.0],[4.0,5.0,6.0],[7.0,8.0,9.0]]
>>> let k = [1.0,2.0,3.0,4.0]
>>> convolvePlain 2 k p
[[37.0,47.0],[67.0,77.0]]


-}

convolvePlain :: Int -> [Double] -> Plain -> Plain
convolvePlain s k ps
  | len < s   = []
  | otherwise = (convolveLine s k p:convolvePlain s k ps')
  where
    len = length ps
    p   = take s ps
    ps' = tail ps

{- |
convolveLine

>>> let k = [0.1,0.2,0.3,0.4]
>>> let p = [[1.0,2.0,3.0,4.0],[5.0,6.0,7.0,8.0]]
>>> convolveLine 2 k p
[4.4,5.4,6.4]
-}

convolveLine :: Int -> [Double] -> [[Double]] -> [Double]
convolveLine s k ps
  | len < s   = []
  | otherwise = ((sum $ zipWith (*) vs k):convolveLine s k ps')
  where
    len = minimum $ map (length) ps
    vs = concat $ map (take s) ps
    ps' = map (tail) ps

