--
-- FullConnLayer : fully connected layer
--

module CNN.FullConnLayer (
  initFilterF
, connect
, deconnect
, reverseFullConnFilter
, updateFullConnFilter
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
>>> let fs2 = []
>>> connect fs2 im
*** Exception: invalid FilterF
>>> connect fs []
*** Exception: invalid Image

-}

connect :: [FilterF] -> Image -> Image
connect [] _ = error "invalid FilterF"
connect _ [] = error "invalid Image"
-- connect fs [[im]] = [[map (dot (1.0:im)) fs]]
connect fs [[im]] = [[map (dot (1.0:im)) fs]]
connect _ [im] = error ("invalid Image 2:" ++ show im)

-- back prop

{- |
deconnect

>>> let im = [[[1.0, 2.0, 3.0]]]
>>> let delta = [1.0, 2.0]
>>> let fs = [[1.0, 2.0],[3.0,4.0],[5.0,6.0]]
>>> let (d,l) = deconnect fs im delta
>>> show d
[]
>>> show l
[]

-}

deconnect :: [FilterF] -> Image -> Delta -> (Delta, Layer)
deconnect fs im delta = (mmul delta fs, FullConnLayer $ calcDiff delta im')
  where
    im' = head $ head im

calcDiff :: Delta -> [Double] -> [FilterF]
calcDiff delta im = map (mulImage im) delta
  where
    mulImage :: [Double] -> Double -> [Double]
    mulImage im d = map (*d) im

-- reverse

reverseFullConnFilter :: [FilterF] -> Layer
reverseFullConnFilter fs = FullConnLayer $ transpose fs

-- update filter

updateFullConnFilter :: [FilterF] -> [Layer] -> (Layer, [Layer])
updateFullConnFilter fs dl = (FullConnLayer fs, dl)

--averageFilterF = mavg

