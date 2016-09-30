--
-- Algebra: algebra library
--

module CNN.Algebra (
   vsub
,  dot
, transpose
) where

vsub :: [Double] -> [Double] -> [Double]
vsub a b = zipWith (-) a b


dot :: [Double] -> [Double] -> Double
dot a b = sum $ zipWith (*) a b

transpose :: [[Double]] -> [[Double]]
transpose fs
  | a == [] = []
  | otherwise = a:(transpose fs')
  where
    (as, fs') = tr fs
    a = concat as
    tr :: [[Double]] -> ([[Double]], [[Double]])
    tr fs = unzip $ map (splitAt 1) fs

