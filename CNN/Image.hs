--
-- Image : image data structure
--

module CNN.Image (
  Image
, Plain
, Class
, Trainer
, flatten
, unflatten  
) where

import qualified Data.Map as M

type Plain = [[Double]]    -- 2D: X x Y pixels
type Image = [Plain]     -- n dimension

type Class = [Double]    -- teacher vector

type Trainer = (Image, Class)

{- |
flattenImage

  IN: Image (=[[[Double]]])

>>> let im = [[[2.0,3.0],[4.0,5.0]],[[1.0,2.0],[4.0,3.0]],[[2.0,1.0],[6.0,5.0]]]
>>> flatten im
[[[2.0,3.0,4.0,5.0,1.0,2.0,4.0,3.0,2.0,1.0,6.0,5.0]]]

-}

flatten :: Image -> Image
flatten im = [[concat $ concat im]]

{- |
unflattenImage

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

