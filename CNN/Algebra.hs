--
-- Algebra: algebra library
--

module CNN.Algebra (
  vadd
, vsub
, vsum
, vscale
, vdot
, madd
, msub
, msum
, mscale
, mmul
, mdot
, mavg
, transpose
) where

import Data.List (foldl')

-- VECTOR FUNCTIONS

vadd :: [Double] -> [Double] -> [Double]
vadd as [] = as
vadd [] bs = bs
vadd as bs = zipWith (+) as bs

vsub :: [Double] -> [Double] -> [Double]    
vsub as [] = as
vsub [] bs = vscale (-1.0) bs
vsub as bs = zipWith (-) as bs

{- |
vsum

>>> let vs = [[1.0,2.0,3.0],[4.0,5.0,6.0]]
>>> vsum vs
[5.0,7.0,9.0]

-}

vsum :: [[Double]] -> [Double]
vsum = foldr vadd []

vscale :: Double -> [Double] -> [Double]
vscale s = map (* s)

vdot :: [Double] -> [Double] -> Double
vdot a b = sum $ zipWith (*) a b

-- MATRIX FUNCTIONS

{- |
madd

>>> let m1 = [[1.0,10.0,100.0],[2.0,20.0,200.0]]
>>> let m2 = [[3.0,30.0,300.0],[4.0,40.0,400.0]]
>>> madd m1 m2
[[4.0,40.0,400.0],[6.0,60.0,600.0]]

-}

madd :: [[Double]] -> [[Double]] -> [[Double]]
madd m1s []  = m1s
madd [] m2s  = m2s
madd m1s m2s = zipWith vadd m1s m2s

{- |
msub

>>> let m1 = [[3.0,30.0,300.0],[5.0,50.0,500.0]]
>>> let m2 = [[1.0,10.0,100.0],[2.0,20.0,200.0]]
>>> msub m1 m2
[[2.0,20.0,200.0],[3.0,30.0,300.0]]

-}

msub :: [[Double]] -> [[Double]] -> [[Double]]
msub m1s []  = m1s
msub [] m2s  = mscale (-1.0) m2s
msub m1s m2s = zipWith vsub m1s m2s

{- |
msum

>>> let m1 = [[3.0,30.0,300.0],[5.0,50.0,500.0]]
>>> let m2 = [[3.0,30.0,300.0],[4.0,40.0,400.0]]
>>> let m3 = [[1.0,10.0,100.0],[2.0,20.0,200.0]]
>>> msum [m1,m2,m3]
[[7.0,70.0,700.0],[11.0,110.0,1100.0]]

-}

msum :: [[[Double]]] -> [[Double]]
--msum = foldr madd []
msum = foldl' madd []

{- |
mscale

>>> let m0 = [[0.0,1.0,2.0],[3.0,4.0,5.0]]
>>> mscale 2.0 m0
[[0.0,2.0,4.0],[6.0,8.0,10.0]]

-}

mscale :: Double -> [[Double]] -> [[Double]]
mscale s = map (vscale s)

{- |
mmul

>>> let v1 = [3.0,30.0,300.0]
>>> let m1 = [[1.0,10.0,100.0],[2.0,20.0,200.0]]
>>> mmul v1 m1
[30303.0,60606.0]

-}

mmul :: [Double] -> [[Double]] -> [Double]
mmul vs = map (vdot vs)

{- |
mdot

>>> let m1 = [[1.0,2.0],[3.0,4.0]]
>>> let m2 = [[5.0,6.0],[7.0,8.0]]
>>> mdot m1 m2
70.0

-}

mdot :: [[Double]] -> [[Double]] -> Double
mdot as bs = sum $ zipWith vdot as bs

{- |
mavg

>>> let m1 = [[1.0,2.0],[3.0,4.0]]
>>> let m2 = [[4.0,3.0],[2.0,1.0]]
>>> mavg [m1,m2]
[[2.5,2.5],[2.5,2.5]]
>>> let m1 = [[1.0,2.0,3.0],[4.0,5.0,6.0]]
>>> let m2 = [[6.0,5.0,4.0],[3.0,2.0,1.0]]
>>> let m3 = [[2.0,2.0,2.0],[2.0,2.0,2.0]]
>>> mavg [m1,m2,m3]
[[3.0,3.0,3.0],[3.0,3.0,3.0]]

-}

mavg :: [[[Double]]] -> [[Double]]
mavg ms = mscale a ss
  where
    a = 1.0 / fromIntegral (length ms)
    ss = msum ms

{- |
transpose

>>> let m1 = [[1.0, 2.0], [3.0, 4.0]]
>>> transpose m1
[[1.0,3.0],[2.0,4.0]]
>>> let m2 = [[1.0, 2.0, 3.0], [4.0, 5.0, 6.0], [7.0, 8.0, 9.0]]
>>> transpose m2
[[1.0,4.0,7.0],[2.0,5.0,8.0],[3.0,6.0,9.0]]
>>> let m3 = [[1.0, 2.0, 3.0, 10.0], [4.0, 5.0, 6.0, 11.0], [7.0, 8.0, 9.0, 12.0]]
>>> transpose m3
[[1.0,4.0,7.0],[2.0,5.0,8.0],[3.0,6.0,9.0],[10.0,11.0,12.0]]

-}

transpose :: Eq a => [[a]] -> [[a]]
transpose fs
  | null l    = []
  | otherwise = l : transpose fs'
  where
    (ls, fs') = tr fs
    l = concat ls
    tr :: [[a]] -> ([[a]], [[a]])
    tr fs = unzip $ map (splitAt 1) fs

