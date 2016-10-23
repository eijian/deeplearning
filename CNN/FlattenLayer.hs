--
-- FlattenLayer: flatten/unflatten layer
--

module CNN.FlattenLayer (
  flatten
, unflatten
) where

import CNN.Image
import CNN.LayerType

{- |
flatten: flatten image

  IN: Image (=[[[Double]]])

>>> let im = [[[2.0,3.0],[4.0,5.0]],[[1.0,2.0],[4.0,3.0]],[[2.0,1.0],[6.0,5.0]]]
>>> flatten im
[[[2.0,3.0,4.0,5.0,1.0,2.0,4.0,3.0,2.0,1.0,6.0,5.0]]]

-}

flatten :: Image -> Image
flatten im = [[concat $ concat im]]

{- |
unflatten : unflatten image

  IN: [[[Double]]]

>>> let ds = [[[2.0,3.0,4.0,5.0,1.0,2.0,4.0,3.0,2.0,1.0,6.0,5.0]]]
>>> unflatten 2 2 ds
[[[2.0,3.0],[4.0,5.0]],[[1.0,2.0],[4.0,3.0]],[[2.0,1.0],[6.0,5.0]]]

-}

unflatten :: Int -> Int -> Image -> Image
unflatten x y [[ds]] = split y $ split x ds
  where
    split :: Int -> [a] -> [[a]]
    split _ [] = []
    split n ds = (d:split n ds')
      where
        (d, ds') = splitAt n ds

