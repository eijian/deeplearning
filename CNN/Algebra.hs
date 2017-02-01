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
import Numeric.LinearAlgebra
--import Numeric.LinearAlgebra.Data

-- VECTOR FUNCTIONS

vadd :: Vector R -> Vector R -> Vector R
vadd = (+)

vsub :: Vector R -> Vector R -> Vector R
vsub = (-)

{- |
vsum

>>> let vs = [vector [1.0,2.0,3.0], vector [4.0,5.0,6.0]]
>>> vsum vs
[5.0,7.0,9.0]

-}

vsum :: [Vector R] -> Vector R
vsum [] = fromList [0.0]
vsum (v:vs) = v + (vsum vs)

vscale :: Double -> Vector R -> Vector R
vscale s a = scale s a

vdot :: Vector R -> Vector R -> Double
vdot a b = a `dot` b

-- MATRIX FUNCTIONS

{- |
madd

>>> let m1 = fromLists [[1.0,10.0,100.0],[2.0,20.0,200.0]]
>>> let m2 = fromLists [[3.0,30.0,300.0],[4.0,40.0,400.0]]
>>> madd m1 m2
(2><3)
 [ 4.0, 40.0, 400.0
 , 6.0, 60.0, 600.0 ]

-}

madd :: Matrix R -> Matrix R -> Matrix R
madd a b = a + b

{- |
msub

>>> let m1 = fromLists [[3.0,30.0,300.0],[5.0,50.0,500.0]]
>>> let m2 = fromLists [[1.0,10.0,100.0],[2.0,20.0,200.0]]
>>> msub m1 m2
(2><3)
 [ 2.0, 20.0, 200.0
 , 3.0, 30.0, 300.0 ]

-}

msub :: Matrix R -> Matrix R -> Matrix R
msub a b = a - b

{- |
msum

>>> let m1 = fromLists [[3.0,30.0,300.0],[5.0,50.0,500.0]]
>>> let m2 = fromLists [[3.0,30.0,300.0],[4.0,40.0,400.0]]
>>> let m3 = fromLists [[1.0,10.0,100.0],[2.0,20.0,200.0]]
>>> msum [m1,m2,m3]
(2><3)
 [  7.0,  70.0,  700.0
 , 11.0, 110.0, 1100.0 ]

-}

msum :: [Matrix R] -> Matrix R
--msum = foldr madd []
msum = foldl' madd (matrix 1 [0.0])

{- |
mscale

>>> let m0 = fromLists [[0.0,1.0,2.0],[3.0,4.0,5.0]]
>>> mscale 2.0 m0
(2><3)
 [ 0.0, 2.0,  4.0
 , 6.0, 8.0, 10.0 ]

-}

mscale :: Double -> Matrix R -> Matrix R
mscale s m = scale s m

{- |
mmul

>>> let v1 = vector [3.0,30.0,300.0]
>>> let m1 = fromLists [[1.0,10.0,100.0],[2.0,20.0,200.0]]
>>> mmul v1 m1
[30303.0,60606.0]

-}

mmul :: Vector R -> Matrix R -> Vector R
mmul v m = m #> v

{- |
mdot

>>> let m1 = fromLists [[1.0,2.0],[3.0,4.0]]
>>> let m2 = fromLists [[5.0,6.0],[7.0,8.0]]
>>> mdot m1 m2
70.0

-}

mdot :: Matrix R -> Matrix R -> Double
--mdot a b = sum $ concat $ toLists (a * b)
mdot a b = flatten a <.> flatten b

{- |
mavg

>>> let m1 = fromLists [[1.0,2.0],[3.0,4.0]]
>>> let m2 = fromLists [[4.0,3.0],[2.0,1.0]]
>>> mavg [m1,m2]
(2><2)
 [ 2.5, 2.5
 , 2.5, 2.5 ]
>>> let m1 = fromLists [[1.0,2.0,3.0],[4.0,5.0,6.0]]
>>> let m2 = fromLists [[6.0,5.0,4.0],[3.0,2.0,1.0]]
>>> let m3 = fromLists [[2.0,2.0,2.0],[2.0,2.0,2.0]]
>>> mavg [m1,m2,m3]
(2><3)
 [ 3.0, 3.0, 3.0
 , 3.0, 3.0, 3.0 ]

-}

mavg :: [Matrix R] -> Matrix R
mavg ms = mscale a ss
  where
    a = 1.0 / fromIntegral (length ms)
    ss = msum ms

{- |
transpose

>>> let m1 = fromLists [[1.0, 2.0], [3.0, 4.0]] :: Matrix R
>>> tr m1
(2><2)
 [ 1.0, 3.0
 , 2.0, 4.0 ]
>>> let m2 = fromLists [[1.0, 2.0, 3.0], [4.0, 5.0, 6.0], [7.0, 8.0, 9.0]] :: Matrix R
>>> tr m2
(3><3)
 [ 1.0, 4.0, 7.0
 , 2.0, 5.0, 8.0
 , 3.0, 6.0, 9.0 ]
>>> let m3 = fromLists [[1.0, 2.0, 3.0, 10.0], [4.0, 5.0, 6.0, 11.0], [7.0, 8.0, 9.0, 12.0]] :: Matrix R
>>> tr m3
(4><3)
 [  1.0,  4.0,  7.0
 ,  2.0,  5.0,  8.0
 ,  3.0,  6.0,  9.0
 , 10.0, 11.0, 12.0 ]

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

