--
-- FullConnLayer : fully connected layer
--

module CNN.FullConnLayer (
  initFilterF
, zeroFilterF
, connect
, deconnect
, reverseFullConnFilter
, updateFullConnFilter
) where

import Control.Monad hiding (msum)
--import Data.List
import Data.Maybe
import Debug.Trace
import System.Random.Mersenne as MT

import CNN.Algebra
import CNN.Image
import CNN.LayerType

{- |
initFilterF

  IN : #kernel
       #channel

  OUT: filter of fully connected layer

  OUT: filter of fully connected layer

>>> f <- initFilterF 3 20
>>> length f
3
>>> length $ head f
21

-}

initFilterF :: Int -> Int -> IO [FilterF]
initFilterF k c = forM [1..k] $ \i -> initKernel c
  where
    a = 1.0 / fromIntegral c
    --a = 4.0 * sqrt (6.0 / fromIntegral (c + k))
    initKernel :: Int -> IO FilterF
    initKernel c = do
      w <- forM [1..c] $ \i -> do
        r <- MT.randomIO :: IO Double
        return ((r * 2.0 - 1.0) * a)
      return (0.0:w)

zeroFilterF :: Int -> Int -> IO [FilterF]
zeroFilterF k c = return $ replicate k (replicate c 0.0)
     
--

{- |
connect

  IN : filter of fully connected layer
       image

  OUT: updated image

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
connect fs [[im]] = [[map (vdot (1.0:im)) fs]]
connect _ [im] = error ("invalid Image 2:" ++ show im)

-- back prop

{- |
deconnect

  IN : filter of fully connected layer
       image
       difference from previous layer

  OUT: difference and updated layer

>>> let im = [[[1.0, 2.0, 3.0]]]
>>> let delta = [[[1.0, 2.0]]]
>>> let fs = [[1.0, 2.0],[3.0,4.0],[5.0,6.0]]
>>> let (d,l) = deconnect fs im delta
>>> d
[[[11.0,17.0]]]
>>> l
FullConnLayer:[[1.0,1.0,2.0,3.0],[2.0,2.0,4.0,6.0]]

-}

deconnect :: [FilterF] -> Image -> Delta -> (Delta, Maybe Layer)
deconnect fs im delta = ([[mmul dl fs']], Just (FullConnLayer $ calcDiff dl im'))
  where
    dl  = head $ head delta
    fs' = tail fs
    im' = head $ head im

calcDiff :: [Double] -> [Double] -> [FilterF]
calcDiff delta im = map (mulImage im') delta
  where
    im' = 1.0:im
    mulImage :: [Double] -> Double -> [Double]
    mulImage im d = map (*d) im'

-- reverse

reverseFullConnFilter :: [FilterF] -> Layer
reverseFullConnFilter fs = FullConnLayer $ transpose fs

-- update filter

updateFullConnFilter :: [FilterF] -> Double -> [Maybe Layer] -> Layer
updateFullConnFilter fs lr dl
  | dl' == [] = FullConnLayer fs
  | otherwise = FullConnLayer fs'
  where
    dl' = catMaybes dl
    delta = mscale (lr / fromIntegral (length dl')) (msum $ strip dl')
    fs' = msub fs delta

strip :: [Layer] -> [[FilterF]]
strip [] = []
strip (FullConnLayer fs:ds) = fs:strip ds
strip (_:ds) = strip ds

