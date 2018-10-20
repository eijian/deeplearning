--
-- ActLayer: activate layer
--

module CNN.ActLayer (
  relu
, relu'
, step
, softmax
, activate
, deactivate
, reverseActFunc
, funcToNum
, numToFunc
) where

import Numeric.LinearAlgebra hiding (step)
import CNN.Algebra
import CNN.Image
import CNN.LayerType

{- |
step

  IN : matrix

  OUT: matrix

>>> let m = fromLists [[1.0, 0.0, (-1.1), (-0.5), 2.1]] :: Matrix R
>>> step m
(1><5)
 [ 1.0, 0.0, 0.0, 0.0, 1.0 ]

-}

step :: ActFunc
step = cmap f
  where
    f :: Double -> Double
    f a = if a > 0.0 then 1.0 else 0.0

{- |
relu

  IN : array of Double

  OUT: array of Double

>>> relu $ fromLists [[1.0, 0.0, (-1.1), (-0.5), 2.1]]
(1><5)
 [ 1.0, 0.0, 0.0, 0.0, 2.1 ]

-}

relu :: ActFunc
relu = cmap (`max` 0.0)

{- |
relu'

  IN : array of Double

  OUT: array of Double

>>> relu' $ fromLists [[1.0, 0.0, (-1.1), (-0.5), 2.1]]
(1><5)
 [ 1.0, 0.0, 0.0, 0.0, 1.0 ]

-}

relu' :: ActFunc
relu' = cmap (\x -> if x > 0.0 then 1.0 else 0.0)

{- |
softmax

  IN : array of Double

  OUT: normalized array of Double

>>> let a1 = softmax $ fromLists [[1.0, 1.0, 1.0]]
>>> (a1 `atIndex` (0,0)) * 3.0
1.0
>>> let a2 = softmax $ fromLists [[1.0, 3.0, 2.0]]
>>> (a2 `atIndex` (0,0)) < (a2 `atIndex` (0,1))
True
>>> (a2 `atIndex` (0,0)) < (a2 `atIndex` (0,2))
True
>>> (a2 `atIndex` (0,1)) > (a2 `atIndex` (0,2))
True
-}

softmax :: ActFunc
softmax as = mscale (1.0 / sume) es
  where
    amax = maxElement as
    es   = cmap (\x -> exp (x - amax)) as
    sume = sumElements es

{-
activate

  IN : activation function
       image

  OUT: calculated image

-}

activate :: ActFunc -> Image -> Image
activate f = map f

{- |
deactivate

  IN : activation function
       image
       difference from previous layer

  OUT: difference and updated layer

>>> fst $ deactivate relu [fromLists [[1.5,(-2.0)]]] [fromLists [[0.5,0.1]]]
[(1><2)
 [ 0.5, 0.0 ]]

-}

m0 :: Matrix R
m0 = fromLists [[0.0]]
m1 :: Matrix R
m1 = fromLists [[1.0]]

deactivate :: ActFunc -> Image -> Delta -> (Delta, Maybe Layer)
deactivate f im delta
  | c == m0   = (dl', Nothing)
  | c == m1   = ([] , Nothing)
  | otherwise = ([] , Nothing)
  where
    c = f m0
    f' = map relu' im
    dl' = zipWith (*) delta f'

reverseActFunc :: ActFunc -> Layer
reverseActFunc relu = ActLayer step

{- |
>>> funcToNum step
0
>>> funcToNum relu
1
-}

funcToNum :: ActFunc -> Int
funcToNum f = case f of
  step    -> 0
  relu    -> 1
  relu'   -> 2
  softmax -> 3
  _       -> error ("invalid function")

numToFunc :: Int -> ActFunc
numToFunc i = case i of
  0 -> step
  1 -> relu
  2 -> relu'
  3 -> softmax
  _ -> error ("invalid function number: " ++ (show i))

