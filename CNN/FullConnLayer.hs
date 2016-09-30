--
-- FullConnLayer : fully connected layer
--

module CNN.FullConnLayer (
  initFilterF
, connect
, deconnect
, reverseFullConnFilter
) where

import Control.Monad
import System.Random.Mersenne as MT

import CNN.LayerType
import CNN.Image
import CNN.Algebra

{- |
initFilterF

  IN: #kernel
      #channel

-}

initFilterF :: Int -> Int -> IO [FilterF]
initFilterF k c = do
  f <- forM [1..k] $ \i -> do
    f' <- initKernel c
    return f'
  return f
  where
    a = 1.0 / fromIntegral c
    initKernel :: Int -> IO FilterF
    initKernel c = do
      w <- forM [1..c] $ \i -> do
        r <- MT.randomIO :: IO Double
        return ((r * 2.0 - 1.0) * a)
      return (0.0:w)

zeroFilterF :: Int -> Int -> [FilterF]
zeroFilterF k c = take k $ repeat (take c $ repeat 0.0)
     
--

{- |
connect

  IN: filter
      image

>>> let fs = [[0.5,1.0,2.0,3.0],[0.1,4.0,5.0,6.0]]
>>> let im = [[[9.0,8.0,7.0]]]
>>> connect fs im
[[[46.5,118.1]]]

-}

connect :: [FilterF] -> Image -> Image
connect [] _ = error "invalid FilterF"
connect _ [] = error "invalid Image"
connect fs [[im]] = [[map (dot (1.0:im)) fs]]

-- back prop

deconnect :: [FilterF] -> Image -> Delta -> (Delta, Layer)
deconnect fs im d = ([], FullConnLayer (zeroFilterF k c))
  where
    k = length fs
    c = length $ head fs

-- reverse

reverseFullConnFilter :: [FilterF] -> Layer
reverseFullConnFilter fs = FullConnLayer $ transpose fs

{- |
transpose

>>> let m1 = [[1.0, 2.0], [3.0, 4.0]]
>>> transpose m1
[[1.0,3.0],[2.0,4.0]]
>>> let m2 = [[1.0, 2.0, 3.0], [4.0, 5.0, 6.0], [7.0, 8.0, 9.0]]
>>> transpose m2
[[1.0,4.0,7.0],[2.0,5.0,8.0],[3.0,6.0,9.0]]
>>> let m3 = [[1.0, 2.0, 3.0, 10.0], [4.0, 5.0, 6.0, 11.0], [7.0, 8.0, 9.0, 12.0]]
>>> transpose m3
[[1.0,4.0,7.0],[2.0,5.0,8.0],[3.0,6.0,9.0],[10.0,11.0,12.0]]

-}

