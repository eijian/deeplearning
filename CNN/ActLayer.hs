--
-- ActLayer: activate layer
--

module CNN.ActLayer (
  relu
, softmax
, activate
, deactivate
, reverseActFunc
) where

import CNN.Image
import CNN.LayerType

{- |
step

  IN : array of Double

  OUT: array of Double

>>> step [1.0, 0.0, (-1.1), (-0.5), 2.1]
[1.0,0.0,0.0,0.0,1.0]

-}

step :: ActFunc
step as = map f as
  where
    f :: Double -> Double
    f a = if a > 0.0 then 1.0 else 0.0

{- |
relu

  IN : array of Double

  OUT: array of Double

>>> relu [1.0, 0.0, (-1.1), (-0.5), 2.1]
[1.0,0.0,0.0,0.0,2.1]

-}

relu :: ActFunc
relu as = map (\x -> max x 0.0) as

{- |
relu'

  IN : array of Double

  OUT: array of Double

>>> relu' [1.0, 0.0, (-1.1), (-0.5), 2.1]
[1.0,0.0,0.0,0.0,1.0]

-}

relu' :: ActFunc
relu' as = map (\x -> if x > 0.0 then 1.0 else 0.0) as

{- |
softmax

  IN : array of Double

  OUT: normalized array of Double

>>> let a1 = softmax [1.0, 1.0, 1.0]
>>> a1!!0 * 3.0
1.0
>>> let a2 = softmax [1.0, 3.0, 2.0]
>>> a2!!0 < a2!!1
True
>>> a2!!0 < a2!!2
True
>>> a2!!1 > a2!!2
True
-}

softmax :: ActFunc
softmax as = map (\x -> x / sume) es
  where
    amax = maximum as
    es   = map (\x -> exp (x - amax)) as
    sume = sum es

{-
activate

  IN : activation function
       image

  OUT: calculated image

-}

activate :: ActFunc -> Image -> Image
activate f im = map (map f) im

{- |
deactivate

  IN : activation function
       image
       difference from previous layer

  OUT: difference and updated layer

>>> fst $ deactivate relu [[[1.5,(-2.0)]]] [0.5,0.1]
[0.5,0.0]

-}

deactivate :: ActFunc -> Image -> Delta -> (Delta, Layer)
deactivate f im delta
  | c == [0.0]  = ([[zipWith (*) dl f']], ActLayer relu')
  | c == [1.0]  = ([], ActLayer f)
  | otherwise = ([], ActLayer f)
  where
    c = f [0.0]
    f' = relu' (head $ head im)
    dl = head $ head delta

reverseActFunc :: ActFunc -> Layer
reverseActFunc relu = ActLayer step

