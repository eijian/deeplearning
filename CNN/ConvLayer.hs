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

import Control.Monad
import Data.List
import Data.Maybe
import Debug.Trace
import System.Random.Mersenne as MT

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
    ro = (fromIntegral (n * n * k)) / (fromIntegral (m * m))
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
>>> let x = [[[0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.0,0.5,0.5,0.5],                             [0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5],                             [0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5],                             [0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5],                             [0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.5,0.5,0.0],                             [0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.5,0.0,0.0,0.0,0.0],                             [0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0],                             [0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.5,0.0,0.0],                             [0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.5,0.0,0.0,0.0],                             [0.0,0.5,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0],                             [0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0],                             [0.0,0.0,0.0,0.0,0.5,0.0,0.0,0.0,0.0,0.0,0.5,0.0]]] :: Image
>>> let fc1 = [([[ 0.085905  , -0.0678689 ,  0.3814169,                                           0.43463653,  0.04209257,  0.20257941,                                         -0.41531846,  0.31991767,  0.43873657]],0.0),                               ([[-0.15370686,  0.30818845,  0.09809999,                                         -0.05181358,  0.02393685,  0.24620931,                                         -0.21878266, -0.0326504 ,  0.30548354]],0.0),                               ([[ 0.15663988, -0.11626568, -0.31853248,                                         -0.08551628,  0.37949085,  0.15236294,                                         -0.07106829,  0.06041856, -0.40324084]],0.0),                               ([[ 0.43609989,  0.10470975,  0.2458974 ,                                          0.3795274 , -0.13935709,  0.27390678,                                          0.3279409 ,  0.14605017,  0.07812175]],0.0),                               ([[ 0.34610384, -0.04524003, -0.22491246,                                          0.27643535,  0.17346834, -0.0795374 ,                                         -0.43491274,  0.39947938,  0.09260994]],0.0),                               ([[-0.43418139, -0.00132555,  0.08678753,                                         -0.1758468 , -0.03364892, -0.00387168,                                          0.07537   ,  0.32523807,  0.29232075]],0.0),                               ([[ 0.2451028 ,  0.04261545,  0.09982441,                                          0.20612988,  0.05839701,  0.24215263,                                          0.42810306,  0.25446708,  0.10060225]],0.0),                               ([[-0.30066952, -0.02077727, -0.19036335,                                         -0.3046046 ,  0.24392145, -0.3880607 ,                                         -0.37032283,  0.14451058,  0.02054084]],0.0),                               ([[-0.04715565, -0.16996655, -0.42473247,                                         -0.18167216, -0.20431407, -0.40762712,                                          0.26751916, -0.14623689,  0.34481431]],0.0),                               ([[ 0.27307444,  0.40835462,  0.40273468,                                          0.08021495, -0.16585497,  0.36816116,                                         -0.39537489, -0.24131427, -0.12080253]],0.0)]
>>> convolve 3 fc1 x
[1.0]
>>> convolve 2 filter img2
[1.0]

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
    vs  = concat $ map (take s) ps
    ps' = map (tail) ps

-- back prop

deconvolve :: Int -> [FilterC] -> Image -> Delta -> (Delta, Layer)
deconvolve s fs im d = ([], ConvLayer s fs)


-- reverse

reverseConvFilter :: Int -> [FilterC] -> Layer
reverseConvFilter s fs = ConvLayer s fs

-- update

updateConvFilter :: Int -> [FilterC] -> Double -> [Layer] -> Layer
updateConvFilter s fs lr dl = ConvLayer s fs
