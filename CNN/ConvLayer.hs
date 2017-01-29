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
import Numeric.LinearAlgebra
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
      return $ fromLists w

--

{- |
convolve

  IN: kernel size
      filter
      image

test: filter k=3, c=2, s=2
>>> let filter = [(fromLists [[1.0,2.0,3.0,4.0],[5.0,6.0,7.0,8.0]],0.25),(fromLists [[3.0,4.0,5.0,6.0],[7.0,8.0,1.0,2.0]],0.5),(fromLists [[5.0,6.0,7.0,8.0],[1.0,2.0,3.0,4.0]],0.75)] :: [FilterC]
>>> let img1 = [fromLists [[1.0,2.0,3.0],[4.0,5.0,6.0],[7.0,8.0,9.0]], fromLists [[4.0,5.0,6.0],[7.0,8.0,9.0],[1.0,2.0,3.0]]] :: Image
>>> let img2 = [fromLists [[1.0,2.0,3.0,4.0],[4.0,5.0,6.0,7.0],[7.0,8.0,9.0,10.0],[10.0,11.0,12.0,13.0]],fromLists [[4.0,5.0,6.0,7.0],[7.0,8.0,9.0,10.0],[1.0,2.0,3.0,4.0],[4.0,5.0,6.0,7.0]]] :: Image
>>> convolve 2 filter img1
[(2><2)
 [ 200.25, 236.25
 , 173.25, 209.25 ],(2><2)
 [ 152.5, 188.5
 , 233.5, 269.5 ],(2><2)
 [ 152.75, 188.75
 , 197.75, 233.75 ]]
>>> convolve 2 filter img2
[(3><3)
 [ 200.25, 236.25, 272.25
 , 173.25, 209.25, 245.25
 , 182.25, 218.25, 254.25 ],(3><3)
 [ 152.5, 188.5, 224.5
 , 233.5, 269.5, 305.5
 , 206.5, 242.5, 278.5 ],(3><3)
 [ 152.75, 188.75, 224.75
 , 197.75, 233.75, 269.75
 , 278.75, 314.75, 350.75 ]]

-}

--
-- USE corr2 ??
--

convolve :: Int -> [FilterC] -> Image -> Image
convolve s fs im = map (convolveImage x iss) fs
  where
    pl = head im
    x  = cols pl - (s - 1)
    y  = rows pl - (s - 1)
    ps = [(i, j) | i <- [0..(x-1)], j <- [0..(y-1)]]
    iss = map subImage im
    subImage :: Plain -> Matrix R
    subImage i = fromColumns $ map (\p -> flatten $ subMatrix p (s, s) i) ps

convolveImage :: Int -> [Matrix R] -> FilterC -> Plain
convolveImage x iss (k, b) = reshape x $ cmap (+ b) $ vsum vs
  where
    vs = zipWith (<#) (toRows k) iss

-- back prop

deconvolve :: Int -> [FilterC] -> Image -> Delta -> (Delta, Maybe Layer)
deconvolve s fs im d = (delta, Just (ConvLayer s dw))
  where
    delta = convolve s fs $ addBorder s d
    sim = slideImage s im
    dw = map (hadamard sim) d

{- |
addBorder

  IN : kernel size
       delta (Image)
  OUT: delta (Image)

>>> let s = [fromLists [[1.0, 2.0],[3.0,4.0]], fromLists [[5.0,6.0],[7.0,8.0]]]
>>> addBorder 2 s
[(4><4)
 [ 0.0, 0.0, 0.0, 0.0
 , 0.0, 1.0, 2.0, 0.0
 , 0.0, 3.0, 4.0, 0.0
 , 0.0, 0.0, 0.0, 0.0 ],(4><4)
 [ 0.0, 0.0, 0.0, 0.0
 , 0.0, 5.0, 6.0, 0.0
 , 0.0, 7.0, 8.0, 0.0
 , 0.0, 0.0, 0.0, 0.0 ]]

-}

addBorder :: Int -> Delta -> Delta
addBorder s d = map ab d
  where
    s' = s - 1
    ab :: Plain -> Plain
    ab p = yb ||| ((xb === (p === xb)) ||| yb)
      where
        x = cols p
        y = rows p
        xb = (s'><x) $ repeat 0.0
        yb = ((y + 2 * s')><s') $ repeat 0.0

{- |
slideImage

>>> let p = [fromLists [[1.0,2.0,3.0,4.0],[5.0,6.0,7.0,8.0],[9.0,10.0,11.0,12.0],[13.0,14.0,15.0,16.0]]] :: Image
>>> slideImage 2 p
[[(3><3)
 [ 1.0,  2.0,  3.0
 , 5.0,  6.0,  7.0
 , 9.0, 10.0, 11.0 ],(3><3)
 [  2.0,  3.0,  4.0
 ,  6.0,  7.0,  8.0
 , 10.0, 11.0, 12.0 ],(3><3)
 [  5.0,  6.0,  7.0
 ,  9.0, 10.0, 11.0
 , 13.0, 14.0, 15.0 ],(3><3)
 [  6.0,  7.0,  8.0
 , 10.0, 11.0, 12.0
 , 14.0, 15.0, 16.0 ]]]

-}

slideImage :: Int -> Image -> [[Plain]]
slideImage s im = map (slidePlain s) im

{- |
slidePlain

>>> let p = fromLists [[1.0,2.0,3.0,4.0],[5.0,6.0,7.0,8.0],[9.0,10.0,11.0,12.0],[13.0,14.0,15.0,16.0]] :: Plain
>>> slidePlain 2 p
[(3><3)
 [ 1.0,  2.0,  3.0
 , 5.0,  6.0,  7.0
 , 9.0, 10.0, 11.0 ],(3><3)
 [  2.0,  3.0,  4.0
 ,  6.0,  7.0,  8.0
 , 10.0, 11.0, 12.0 ],(3><3)
 [  5.0,  6.0,  7.0
 ,  9.0, 10.0, 11.0
 , 13.0, 14.0, 15.0 ],(3><3)
 [  6.0,  7.0,  8.0
 , 10.0, 11.0, 12.0
 , 14.0, 15.0, 16.0 ]]

>>> slidePlain 3 p
[(2><2)
 [ 1.0, 2.0
 , 5.0, 6.0 ],(2><2)
 [ 2.0, 3.0
 , 6.0, 7.0 ],(2><2)
 [ 3.0, 4.0
 , 7.0, 8.0 ],(2><2)
 [ 5.0,  6.0
 , 9.0, 10.0 ],(2><2)
 [  6.0,  7.0
 , 10.0, 11.0 ],(2><2)
 [  7.0,  8.0
 , 11.0, 12.0 ],(2><2)
 [  9.0, 10.0
 , 13.0, 14.0 ],(2><2)
 [ 10.0, 11.0
 , 14.0, 15.0 ],(2><2)
 [ 11.0, 12.0
 , 15.0, 16.0 ]]

-}

slidePlain :: Int -> Plain -> [Plain]
slidePlain s m 
  | s < 1     = []
  | s == 1    = [m]
  | otherwise = map (\p -> subMatrix p (x, y) m) ps
  where
    x = cols m - (s - 1)
    y = rows m - (s - 1)
    ps = [(i, j) | i <- [0..s-1], j <- [0..s-1]]

{- |
hadamard

  IN : slided images
       delta
  OUT: (Kernel, Bias)

>>> let p = [fromLists [[1.0,2.0,3.0,4.0],[5.0,6.0,7.0,8.0],[9.0,10.0,11.0,12.0],[13.0,14.0,15.0,16.0]], fromLists [[16.0,15.0,14.0,13.0],[12.0,11.0,10.0,9.0],[8.0,7.0,6.0,5.0],[4.0,3.0,2.0,1.0]]] :: Image
>>> let ds = fromLists [[0.1,0.2,0.3],[0.4,0.5,0.6],[0.7,0.8,0.9]]
>>> hadamard (slideImage 2 p) ds
((2><4)
 [ 34.800000000000004, 39.3, 52.800000000000004, 57.3
 ,               41.7, 37.2, 23.700000000000006, 19.2 ],4.5)

-}

hadamard :: [[Plain]] -> Plain -> FilterC
hadamard sim ds = (w, b)
  where
    b = sumElements ds
    d' = flatten ds
    w = fromLists $ map (map ((<.>) d' . flatten)) sim

{- |
reverseConvFilter

  IN : kernel size
       original filter
  OUT: reversed filter

>>> let fs = [(fromLists [[0.1,0.2,0.3,0.4],[0.5,0.6,0.7,0.8]],1.0),(fromLists [[0.1,0.3,0.5,0.7],[0.2,0.4,0.6,0.8]],2.0),(fromLists [[0.1,0.2,0.5,0.6],[0.3,0.4,0.7,0.8]],3.0)] :: [FilterC]
>>> let ConvLayer s fs' = reverseConvFilter 2 fs
>>> s
2
>>> fs'
[((3><4)
 [ 0.4, 0.3, 0.2, 0.1
 , 0.7, 0.5, 0.3, 0.1
 , 0.6, 0.5, 0.2, 0.1 ],0.0),((3><4)
 [ 0.8, 0.7, 0.6, 0.5
 , 0.8, 0.6, 0.4, 0.2
 , 0.8, 0.7, 0.4, 0.3 ],0.0)]

-}

reverseConvFilter :: Int -> [FilterC] -> Layer
reverseConvFilter s fs = ConvLayer s (zip rv (repeat 0.0))
  where
    (k, _) = unzip fs
    k'     = map (toLists . fliprl) k
    --r      = transpose $ map (map reverse) k'
    r      = transpose k'
    rv     = map fromLists r

-- update

updateConvFilter :: Int -> [FilterC] -> Double -> [Maybe Layer] -> Layer
updateConvFilter s fs lr dl
  | dl' == [] = ConvLayer s fs
  | otherwise = ConvLayer s $ zip ks' (toList bs')
  where
    dl' = catMaybes dl
    sc = lr / fromIntegral (length dl')
    (ks , bs ) = unzip fs
    (kss, bss) = unzip $ map unzip $ strip dl'
    dbs = vsum $ map fromList bss
    dks = map (mscale sc . msum) $ transpose kss
    bs' = (fromList bs) - (vscale sc dbs)
    ks' = zipWith (-) ks dks

strip :: [Layer] -> [[FilterC]]
strip [] = []
strip (ConvLayer s fs:ds) = fs:strip ds
strip (_:ds) = strip ds  -- if not ConvLayer

