--
-- PoolLayer : pooling layer
--

module CNN.PoolLayer (
  poolMax
, depoolMax
, reversePooling
) where

import CNN.Algebra
import CNN.Image
import CNN.LayerType

type Pix = (Double, Double)

{- |
poolMax

  In : pooling size (x = y)
       image

  OUT: updated image
       position of max values

>>> let im = [[[1.0,2.0,3.0,4.0],[8.0,7.0,6.0,5.0],[1.0,3.0,5.0,7.0],[2.0,4.0,6.0,8.0]]]
>>> poolMax 2 im
[[[[8.0,6.0],[4.0,8.0]]],[[[2.0,2.0],[3.0,3.0]]]]

-}

poolMax :: Int -> Image -> [Image]
poolMax s im = [fst pixs, snd pixs] 
  where
    pixs = unzip $ map unzipPix (poolMax' s im)

unzipPix :: [[Pix]] -> ([[Double]], [[Double]])
unzipPix pixs = unzip $ map unzip pixs

poolMax' :: Int -> Image -> [[[Pix]]]
poolMax' s = map (poolMaxPlain s)

poolMaxPlain :: Int -> Plain -> [[Pix]]
poolMaxPlain s p = map (poolMaxLine s) ls
  where
    ls = splitPlain s p

splitPlain :: Int -> Plain -> [[[Double]]]
splitPlain s [] = []
splitPlain s p  = p' : splitPlain s ps
  where
    (p', ps)  = splitAt s p

{- |
  poolMaxLine

>>> poolMaxLine 2 []
[]
>>> poolMaxLine 2 [[1.0,2.0,3.0,4.0,5.0,6.0],[7.0,8.0,9.0,1.0,2.0,3.0]]
[(8.0,3.0),(9.0,2.0),(6.0,1.0)]
>>> poolMaxLine 3 [[1.0,2.0,3.0,4.0,5.0],[7.0,8.0,9.0,1.0,2.0],[3.0,4.0,5.0,6.0,7.0]]
[(9.0,5.0),(7.0,5.0)]
-}

poolMaxLine :: Int -> [[Double]] -> [Pix]
poolMaxLine _ [] = []
poolMaxLine s ls
  | len == 0  = []
  | otherwise = pixs : poolMaxLine s ls'
  where
    len  = length $ head ls
    pixs = max' $ zip (concatMap (take s) ls) [0.0 ..]
    ls'  = map (drop s) ls

max' :: [Pix] -> Pix
max' [] = error "empty list!"
max' [x] = x
max' (x:xs) = maximum' x (max' xs)

maximum' :: Pix -> Pix -> Pix
maximum' a@(v1, i1) b@(v2, i2) = if v1 < v2 then b else a

-- back prop

{- |
depoolMax

>>> let im = [[[0.0,2.0],[3.0,1.0]]]
>>> let dl = [[[0.1,0.2],[0.3,0.4]]]
>>> depoolMax 2 im dl
([[[0.1,0.0,0.0,0.0],[0.0,0.0,0.2,0.0],[0.0,0.0,0.0,0.4],[0.0,0.3,0.0,0.0]]],MaxPoolLayer:2)

-}

depoolMax :: Int -> Image -> Delta -> (Delta, Layer)
depoolMax s im d = (zipWith (concatWith (expand s 0.0)) im d, MaxPoolLayer s)
  where
    concatWith :: ([Int] -> [Double] -> [[Double]])
               ->  [[Double]] -> [[Double]] -> [[Double]]
    concatWith f i d = concat (zipWith f (map (map truncate) i) d)

{- |
expand

  IN : size of pooling
       filling value
       positions
       delta values

>>> let ps = [1, 3]
>>> let ds = [0.1, 0.2]
>>> expand 2 0.0 ps ds
[[0.0,0.1,0.0,0.0],[0.0,0.0,0.0,0.2]]
-}

expand :: Int -> Double -> [Int] -> [Double] -> [[Double]]
expand s r ps ds = map concat (transpose $ map split' $ zipWith ex ps ds)
  where
    split' = split s
    ex :: Int -> Double -> [Double]
    ex p d = take (s*s) (replicate p r ++ [d] ++ repeat r)

split :: Int -> [a] -> [[a]]
split s [] = []
split s xs
  | length xs < s = [xs]
  | otherwise     = l : split s ls
    where
      (l, ls) = splitAt s xs

-- reverse

reversePooling :: Int -> Layer
reversePooling = MaxPoolLayer


