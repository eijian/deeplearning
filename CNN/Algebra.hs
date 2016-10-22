--
-- Algebra: algebra library
--

module CNN.Algebra (
  vadd
, vsub
, vscale
, vdot
, madd
, msub
, msum
, mscale
, mmul
, mavg
, transpose
) where

vadd :: [Double] -> [Double] -> [Double]    
vadd as bs = zipWith (+) as bs

vsub :: [Double] -> [Double] -> [Double]    
vsub as bs = zipWith (-) as bs

vscale :: Double -> [Double] -> [Double]
vscale s vs = map (* s) vs

vdot :: [Double] -> [Double] -> Double
vdot a b = sum $ zipWith (*) a b

madd :: [[Double]] -> [[Double]] -> [[Double]]
madd m1s []  = m1s
madd [] m2s  = m2s
madd m1s m2s = zipWith vadd m1s m2s

msub :: [[Double]] -> [[Double]] -> [[Double]]
msub m1s []  = m1s
msub [] m2s  = mscale (-1.0) m2s
msub m1s m2s = zipWith vsub m1s m2s

msum :: [[[Double]]] -> [[Double]]
msum [] = []
msum (n:ns) = madd n (msum ns)

{- |
mscale

>>> let m0 = [[0.0,1.0,2.0],[3.0,4.0,5.0]]
>>> mscale 2.0 m0
[[0.0,2.0,4.0],[6.0,8.0,10.0]]

-}

mscale :: Double -> [[Double]] -> [[Double]]
mscale s ms = map (vscale s) ms

mmul :: [Double] -> [[Double]] -> [Double]
mmul vs ms = map (vdot vs) ms

{-
mdot

  make? or not?
-}

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
    a = 1.0 / (fromIntegral $ length ms)
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
  | l == [] = []
  | otherwise = l:(transpose fs')
  where
    (ls, fs') = tr fs
    l = concat ls
    tr :: [[a]] -> ([[a]], [[a]])
    tr fs = unzip $ map (splitAt 1) fs

