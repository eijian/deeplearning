--
-- Algebra: algebra library
--

module CNN.Algebra (
   vsub
,  dot
, transpose
, mavg
, mmul
, madd
, mscale
) where

vsub :: [Double] -> [Double] -> [Double]
vsub a b = zipWith (-) a b


dot :: [Double] -> [Double] -> Double
dot a b = sum $ zipWith (*) a b

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
    ss = sum ms
    sum :: [[[Double]]] -> [[Double]]
    sum [] = []
    sum (n:ns) = madd n (sum ns)

madd :: [[Double]] -> [[Double]] -> [[Double]]
madd m1s []  = m1s
madd [] m2s  = m2s
madd m1s m2s = zipWith vadd m1s m2s

vadd :: [Double] -> [Double] -> [Double]    
vadd as bs = zipWith (+) as bs

{- |
mscale

>>> let m0 = [[0.0,1.0,2.0],[3.0,4.0,5.0]]
>>> mscale 2.0 m0
[[0.0,2.0,4.0],[6.0,8.0,10.0]]

-}

mscale :: Double -> [[Double]] -> [[Double]]
mscale s ms = map (vscale s) ms

vscale :: Double -> [Double] -> [Double]
vscale s vs = map (* s) vs

mmul :: [Double] -> [[Double]] -> [Double]
mmul vs ms = map (dot vs) ms

