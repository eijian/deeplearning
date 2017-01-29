--
-- PoolLayer : pooling layer
--

module CNN.PoolLayer (
  poolMax
, depoolMax
, reversePooling
) where

import Numeric.LinearAlgebra

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

>>> let im = [fromLists [[1.0,2.0,3.0,4.0],[8.0,7.0,6.0,5.0],[1.0,3.0,5.0,7.0],[2.0,4.0,6.0,8.0]], fromLists [[1.0,2.0,3.0,4.0],[2.0,2.0,3.0,4.0],[3.0,3.0,3.0,4.0],[4.0,4.0,4.0,4.0]]]
>>> poolMax 2 im
[[(2><2)
 [ 8.0, 6.0
 , 4.0, 8.0 ],(2><2)
 [ 2.0, 4.0
 , 4.0, 4.0 ]],[(2><2)
 [ 2.0, 2.0
 , 3.0, 3.0 ],(2><2)
 [ 1.0, 1.0
 , 2.0, 1.0 ]]]

-}

poolMax :: Int -> Image -> [Image]
poolMax s im = [os, is] 
  where
    pl = head im
    x  = cols pl `div` s
    y  = rows pl `div` s
    ps = [(i*s, j*s) | i <- [0..(x-1)], j <- [0..(y-1)]]
    (os, is) = unzip $ map (toPlain x y . maxPix s ps) im

toPlain :: Int -> Int -> [Pix] -> (Plain, Plain)
toPlain x y pls = ((x><y) op, (x><y) ip)
  where
    (op, ip) = unzip pls

maxPix :: Int -> [(Int, Int)] -> Plain -> [Pix]
maxPix s ps is = map (max' . toPix) ps
  where
    toPix :: (Int, Int) -> [Pix]
    toPix p = zip (concat $ toLists $ subMatrix p (s, s) is) [0.0..]

max' :: [Pix] -> Pix
max' [] = error "empty list!"
max' [x] = x
max' (x:xs) = maximum' x (max' xs)

maximum' :: Pix -> Pix -> Pix
maximum' a@(v1, _) b@(v2, _) = if v1 < v2 then b else a

-- back prop

{- |
depoolMax

>>> let im = [fromLists [[0.0,2.0],[3.0,1.0]]]
>>> let dl = [fromLists [[0.1,0.2],[0.3,0.4]]]
>>> depoolMax 2 im dl
([(4><4)
 [ 0.1, 0.0, 0.0, 0.0
 , 0.0, 0.0, 0.2, 0.0
 , 0.0, 0.0, 0.0, 0.4
 , 0.0, 0.3, 0.0, 0.0 ]],Nothing)

-}

depoolMax :: Int -> Image -> Delta -> (Delta, Maybe Layer)
depoolMax s im d = (zipWith (concatWith (expand s 0.0)) im d, Nothing)
  where
    concatWith :: (Double -> Double -> Matrix R) ->  Matrix R -> Matrix R
               -> Matrix R
    concatWith f i d = fromBlocks $ zipWith (zipWith f) is ds
      where
        is = toLists i
        ds = toLists d

{- |
expand

  IN : size of pooling
       filling value
       positions
       delta values

>>> expand 2 0.0 0 3
(2><2)
 [ 3.0, 0.0
 , 0.0, 0.0 ]
>>> expand 2 0.0 2 4
(2><2)
 [ 0.0, 0.0
 , 4.0, 0.0 ]
>>> expand 3 0.0 5 3
(3><3)
 [ 0.0, 0.0, 0.0
 , 0.0, 0.0, 3.0
 , 0.0, 0.0, 0.0 ]
>>> expand 3 0.0 8 3
(3><3)
 [ 0.0, 0.0, 0.0
 , 0.0, 0.0, 0.0
 , 0.0, 0.0, 3.0 ]

-}

expand :: Int -> Double -> Double -> Double -> Matrix R
expand s r p d = (s><s) $ (replicate p1 r ++ [d] ++ replicate p2 r)
  where
    p1 = truncate p
    p2 = max 0 (s * s - p1 - 1)

-- reverse

reversePooling :: Int -> Layer
reversePooling = MaxPoolLayer


