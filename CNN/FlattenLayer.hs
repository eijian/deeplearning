--
-- FlattenLayer: flatten/unflatten layer
--

module CNN.FlattenLayer (
  flatten
, unflatten
) where

import qualified Numeric.LinearAlgebra as A

import CNN.Image
import CNN.LayerType

{- |
flatten: flatten image

  IN: Image (=[Matrix R])

>>> let im = [A.fromLists [[2.0,3.0],[4.0,5.0]], A.fromLists [[1.0,2.0],[4.0,3.0]], A.fromLists [[2.0,1.0],[6.0,5.0]]]
>>> flatten im
[(1><12)
 [ 2.0, 3.0, 4.0, 5.0, 1.0, 2.0, 4.0, 3.0, 2.0, 1.0, 6.0, 5.0 ]]

-}

flatten :: Image -> Image
flatten im = [A.fromLists [concat $ map (A.toList . A.flatten) im]]

{- |
unflatten: unflatten image

  IN: [Matrix R]

>>> let ds = [A.fromLists [[2.0,3.0,4.0,5.0,1.0,2.0,4.0,3.0,2.0,1.0,6.0,5.0]]]
>>> unflatten 2 2 ds
[(2><2)
 [ 2.0, 3.0
 , 4.0, 5.0 ],(2><2)
 [ 1.0, 2.0
 , 4.0, 3.0 ],(2><2)
 [ 2.0, 1.0
 , 6.0, 5.0 ]]

-}

unflatten :: Int -> Int -> Image -> Image
unflatten x y [m] = map (A.reshape x . A.fromList) $ split (y*x) v
  where
    v = A.toList $ A.flatten m
    split :: Int -> [a] -> [[a]]
    split _ [] = []
    split n ds = d : split n ds'
      where
        (d, ds') = splitAt n ds