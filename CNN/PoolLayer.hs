--
-- PoolLayer : pooling layer
--

module CNN.PoolLayer (
  poolMax
, depoolMax
, reversePooling
) where

import CNN.LayerType
import CNN.Image

type Pix = (Double, Int)

poolMax :: Int -> Image -> Image
poolMax s im = fst $ unzip $ map unzipPix (poolMax' s im)

unzipPix :: [[Pix]] -> ([[Double]], [[Int]])
unzipPix pixs = unzip $ map unzip pixs

poolMax' :: Int -> Image -> [[[Pix]]]
poolMax' s im = map (poolMaxPlain s) im

poolMaxPlain :: Int -> Plain -> [[Pix]]
poolMaxPlain s p = map (poolMaxLine s) ls
  where
    ls = splitPlain s p

splitPlain :: Int -> Plain -> [[[Double]]]
splitPlain s [] = []
splitPlain s p  = (p':splitPlain s ps)
  where
    (p', ps)  = splitAt s p

{- |
  poolMaxLine

>>> poolMaxLine 2 []
[]
>>> poolMaxLine 2 [[1.0,2.0,3.0,4.0,5.0,6.0],[7.0,8.0,9.0,1.0,2.0,3.0]]
[(8.0,4),(9.0,3),(6.0,2)]
>>> poolMaxLine 3 [[1.0,2.0,3.0,4.0,5.0],[7.0,8.0,9.0,1.0,2.0],[3.0,4.0,5.0,6.0,7.0]]
[(9.0,6),(7.0,6)]
-}

poolMaxLine :: Int -> [[Double]] -> [Pix]
poolMaxLine _ [] = []
poolMaxLine s ls
  | len == 0  = []
  | otherwise = (pixs:poolMaxLine s ls')
  where
    len  = length $ head ls
    pixs = max' $ zip (concat $ map (take s) ls) [1..]
    ls'  = map (drop s) ls

max' :: [Pix] -> Pix
max' [] = error "empty list!"
max' [x] = x
max' (x:xs) = maximum' x (max' xs)

maximum' :: Pix -> Pix -> Pix
maximum' a@(v1, i1) b@(v2, i2) = if v1 < v2 then b else a

-- back prop

depoolMax :: Int -> Image -> Delta -> (Delta, Layer)
depoolMax s im d = ([], MaxPoolLayer s)

-- reverse

reversePooling :: Int -> Layer
reversePooling s = MaxPoolLayer s


