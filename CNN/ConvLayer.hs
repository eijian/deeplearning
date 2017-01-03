--
-- ConvLayer : convolution layer
--

module CNN.ConvLayer (
  initFilterC
, convolve
, deconvolve
, reverseConvFilter
, updateConvFilter
) where

import Control.Monad hiding (msum)
import Data.List hiding (transpose)
import Data.Maybe
import Debug.Trace
import System.Random.Mersenne as MT

import CNN.Algebra
import CNN.Image
import CNN.LayerType

{- |
initFilterC

  IN : #kernel
       #channel
       input size (x * y)
       kernel size (n * n)
       pool size (m * m)

  OUT: filter of convolution layer

-}

initFilterC :: Int -> Int -> Int -> Int -> Int -> Int -> IO [FilterC]
initFilterC k c x y n m = do
  f <- forM [1..k] $ \i -> do
    w <- initKernel c n
    return (w, 0.0)
  return f
  where
    ri = fromIntegral (n * n * c)   -- resolution (input)
    ro = fromIntegral (n * n * k) / (fromIntegral (m * m))
    a = sqrt (6.0 / (ri + ro))
    initKernel :: Int -> Int -> IO Kernel
    initKernel c n = do
      let
        sz = n * n
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
>>> convolve 2 filter img1
[[[200.25,236.25],[173.25,209.25]],[[152.5,188.5],[233.5,269.5]],[[152.75,188.75],[197.75,233.75]]]
>>> convolve 2 filter img2
[[[200.25,236.25,272.25],[173.25,209.25,245.25],[182.25,218.25,254.25]],[[152.5,188.5,224.5],[233.5,269.5,305.5],[206.5,242.5,278.5]],[[152.75,188.75,224.75],[197.75,233.75,269.75],[278.75,314.75,350.75]]]

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
    f (a:[]) (b:_)  = [a + b]
    f (a:as) (b:bs) = (a + b) : f as bs

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
  | otherwise = convolveLine s k p : convolvePlain s k ps'
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
  | otherwise = sum (zipWith (*) vs k) : convolveLine s k ps'
  where
    len = minimum $ map length ps
    vs  = concatMap (take s) ps
    ps' = map tail ps

-- back prop

deconvolve :: Int -> [FilterC] -> Image -> Delta -> (Delta, Layer)
deconvolve s fs im d = (delta, ConvLayer s (zip dw db))
  where
    delta = convolve s fs $ addBorder s d
    db = map (sum . map sum) d  -- delta B
    sim = slideImage s im
    dw = map (hadamard sim) d

{- |
addBorder

  IN : kernel size
       delta (Image)
  OUT: delta (Image)

>>> let s = [[[1.0, 2.0],[3.0,4.0]],[[5.0,6.0],[7.0,8.0]]]
>>> addBorder 2 s
[[[0.0,0.0,0.0,0.0],[0.0,1.0,2.0,0.0],[0.0,3.0,4.0,0.0],[0.0,0.0,0.0,0.0]],[[0.0,0.0,0.0,0.0],[0.0,5.0,6.0,0.0],[0.0,7.0,8.0,0.0],[0.0,0.0,0.0,0.0]]]

-}

addBorder :: Int -> Delta -> Delta
addBorder s d = map ab d
  where
    xb = replicate (s - 1) 0.0
    l  = length (head $ head d) + (s - 1) * 2
    yb = replicate (s - 1) (replicate l 0.0)
    ab :: [[Double]] -> [[Double]]
    ab y = yb ++ map (\x -> xb ++ x ++ xb) y ++ yb

{- |
slideImage

>>> let p = [[[1.0,2.0,3.0,4.0],[5.0,6.0,7.0,8.0],[9.0,10.0,11.0,12.0],[13.0,14.0,15.0,16.0]]] :: Image
>>> slideImage 2 p
[[[[1.0,2.0,3.0,4.0],[5.0,6.0,7.0,8.0],[9.0,10.0,11.0,12.0],[13.0,14.0,15.0,16.0]],[[2.0,3.0,4.0],[6.0,7.0,8.0],[10.0,11.0,12.0],[14.0,15.0,16.0]],[[5.0,6.0,7.0,8.0],[9.0,10.0,11.0,12.0],[13.0,14.0,15.0,16.0]],[[6.0,7.0,8.0],[10.0,11.0,12.0],[14.0,15.0,16.0]]]]
-}

slideImage :: Int -> Image -> [[Plain]]
slideImage s im = map (concat . slidePlains s . slidePlain s) im

{- |
slidePlain

>>> let p = [[1.0,2.0,3.0,4.0],[5.0,6.0,7.0,8.0],[9.0,10.0,11.0,12.0],[13.0,14.0,15.0,16.0]] :: Plain
>>> slidePlain 2 p
[[[1.0,2.0,3.0,4.0],[5.0,6.0,7.0,8.0],[9.0,10.0,11.0,12.0],[13.0,14.0,15.0,16.0]],[[2.0,3.0,4.0],[6.0,7.0,8.0],[10.0,11.0,12.0],[14.0,15.0,16.0]]]
>>> slidePlain 3 p
[[[1.0,2.0,3.0,4.0],[5.0,6.0,7.0,8.0],[9.0,10.0,11.0,12.0],[13.0,14.0,15.0,16.0]],[[2.0,3.0,4.0],[6.0,7.0,8.0],[10.0,11.0,12.0],[14.0,15.0,16.0]],[[3.0,4.0],[7.0,8.0],[11.0,12.0],[15.0,16.0]]]

-}

slidePlain :: Int -> Plain -> [Plain]
slidePlain s p
  | s < 1     = []
  | s == 1    = [p]
  | otherwise = ps ++ [p']
  where
    ps = slidePlain (s - 1) p
    p' = map tail $ last ps

{- |
slidePlains

>>> let ps2 = [[[1.0,2.0,3.0,4.0],[5.0,6.0,7.0,8.0],[9.0,10.0,11.0,12.0],[13.0,14.0,15.0,16.0]],[[2.0,3.0,4.0],[6.0,7.0,8.0],[10.0,11.0,12.0],[14.0,15.0,16.0]]]
>>> slidePlains 2 ps2
[[[[1.0,2.0,3.0,4.0],[5.0,6.0,7.0,8.0],[9.0,10.0,11.0,12.0],[13.0,14.0,15.0,16.0]],[[2.0,3.0,4.0],[6.0,7.0,8.0],[10.0,11.0,12.0],[14.0,15.0,16.0]]],[[[5.0,6.0,7.0,8.0],[9.0,10.0,11.0,12.0],[13.0,14.0,15.0,16.0]],[[6.0,7.0,8.0],[10.0,11.0,12.0],[14.0,15.0,16.0]]]]
>>> let ps3 = [[[1.0,2.0,3.0,4.0],[5.0,6.0,7.0,8.0],[9.0,10.0,11.0,12.0],[13.0,14.0,15.0,16.0]],[[2.0,3.0,4.0],[6.0,7.0,8.0],[10.0,11.0,12.0],[14.0,15.0,16.0]],[[3.0,4.0],[7.0,8.0],[11.0,12.0],[15.0,16.0]]]
>>> slidePlains 3 ps3
[[[[1.0,2.0,3.0,4.0],[5.0,6.0,7.0,8.0],[9.0,10.0,11.0,12.0],[13.0,14.0,15.0,16.0]],[[2.0,3.0,4.0],[6.0,7.0,8.0],[10.0,11.0,12.0],[14.0,15.0,16.0]],[[3.0,4.0],[7.0,8.0],[11.0,12.0],[15.0,16.0]]],[[[5.0,6.0,7.0,8.0],[9.0,10.0,11.0,12.0],[13.0,14.0,15.0,16.0]],[[6.0,7.0,8.0],[10.0,11.0,12.0],[14.0,15.0,16.0]],[[7.0,8.0],[11.0,12.0],[15.0,16.0]]],[[[9.0,10.0,11.0,12.0],[13.0,14.0,15.0,16.0]],[[10.0,11.0,12.0],[14.0,15.0,16.0]],[[11.0,12.0],[15.0,16.0]]]]

-}

slidePlains :: Int -> [Plain] -> [[Plain]]
slidePlains s ps
  | s < 1     = []
  | s == 1    = [ps]
  | otherwise = pss ++ [ps']
  where
    pss = slidePlains (s - 1) ps
    ps' = map tail $ last pss

{- |
hadamard

>>> let p = [[[1.0,2.0,3.0,4.0],[5.0,6.0,7.0,8.0],[9.0,10.0,11.0,12.0],[13.0,14.0,15.0,16.0]],[[16.0,15.0,14.0,13.0],[12.0,11.0,10.0,9.0],[8.0,7.0,6.0,5.0],[4.0,3.0,2.0,1.0]]] :: Image
>>> let ds = [[0.1,0.2,0.3],[0.4,0.5,0.6],[0.7,0.8,0.9]]
>>> hadamard (slideImage 2 p) ds
[[34.800000000000004,39.3,52.8,57.3],[41.7,37.2,23.700000000000003,19.200000000000003]]

-}

hadamard :: [[Plain]] -> Plain -> [[Double]]
hadamard sim ds = map (map (mdot ds)) sim

{- |
reverseConvFilter

  IN : kernel size
       original filter
  OUT: reversed filter

>>> let fs = [([[0.1,0.2,0.3,0.4],[0.5,0.6,0.7,0.8]],1.0),([[0.1,0.3,0.5,0.7],[0.2,0.4,0.6,0.8]],2.0),([[0.1,0.2,0.5,0.6],[0.3,0.4,0.7,0.8]],3.0)] :: [FilterC]
>>> let ConvLayer s fs' = reverseConvFilter 2 fs
>>> s
2
>>> fs'
[([[0.4,0.3,0.2,0.1],[0.7,0.5,0.3,0.1],[0.6,0.5,0.2,0.1]],0.0),([[0.8,0.7,0.6,0.5],[0.8,0.6,0.4,0.2],[0.8,0.7,0.4,0.3]],0.0)]

-}

reverseConvFilter :: Int -> [FilterC] -> Layer
reverseConvFilter s fs = ConvLayer s (zip rv (repeat 0.0))
  where
    (k, _) = unzip fs
    rv     = transpose $ map (map reverse) k

-- update

updateConvFilter :: Int -> [FilterC] -> Double -> [Layer] -> Layer
updateConvFilter s fs lr dl = ConvLayer s $ zip ks' bs'
  where
    sc = lr / fromIntegral (length dl)
    (ks , bs ) = unzip fs
    (kss, bss) = unzip $ map unzip $ strip dl
    dbs = vscale sc $ vsum bss
    dks = map (mscale sc . msum) $ transpose kss
    bs' = vsub bs dbs
    ks' = zipWith msub ks dks

strip :: [Layer] -> [[FilterC]]
strip [] = []
strip (ConvLayer s fs:ds) = fs:strip ds
strip (_:ds) = strip ds  -- if not ConvLayer

