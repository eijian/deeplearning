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
import Numeric.LinearAlgebra
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
>>> rows f
3
>>> cols f
21

-}

initFilterF :: Int -> Int -> IO FilterF
initFilterF k c = do
  rs <- forM [1..k] $ \i -> do
    w <- forM [1..c] $ \j -> do
      r <- MT.randomIO :: IO Double
      return ((r * 2.0 - 1.0) * a)
    return (0.0:w)
  return $ fromLists rs
  where
    a = 1.0 / fromIntegral c
    --a = 4.0 * sqrt (6.0 / fromIntegral (c + k))

zeroFilterF :: Int -> Int -> IO FilterF
zeroFilterF k c = return $ (k><(c+1)) $ repeat 0.0
     
--

{- |
connect

  IN : filter of fully connected layer
       image

  OUT: updated image

>>> let fs = fromLists [[0.5,1.0,2.0,3.0],[0.1,4.0,5.0,6.0]]
>>> let im = [fromLists [[9.0,8.0,7.0]]]
>>> connect fs im
[(1><2)
 [ 46.5, 118.1 ]]
>>> connect fs []
*** Exception: invalid Image

-}

connect :: FilterF -> Image -> Image
connect _ [] = error "invalid Image"
connect fs [im] = [reshape (rows fs) $ fs #> v]
  where
    v = vjoin [konst 1.0 1, flatten im]

-- back prop

{- |
deconnect

  IN : filter of fully connected layer
       image
       difference from previous layer

  OUT: difference and updated layer

>>> let im = [fromLists [[1.0, 2.0, 3.0]]]
>>> let delta = [fromLists [[1.0, 2.0]]]
>>> let fs = fromLists [[1.0, 2.0],[3.0,4.0],[5.0,6.0]]
>>> let (d,l) = deconnect fs im delta
>>> d
[(1><2)
 [ 11.0, 17.0 ]]
>>> l
Just FullConnLayer:(2><4)
 [ 1.0, 1.0, 2.0, 3.0
 , 2.0, 2.0, 4.0, 6.0 ]

-}

deconnect :: FilterF -> Image -> Delta -> (Delta, Maybe Layer)
deconnect fs im delta = ([reshape (rows fs') $ fs' #> dl]
                        , Just (FullConnLayer $ outer dl im'))
  where
    dl  = flatten $ head delta    -- to Vector
    fs' = dropRows 1 fs
    im' = vjoin [konst 1.0 1, flatten $ head im]

-- reverse

reverseFullConnFilter :: FilterF -> Layer
reverseFullConnFilter fs = FullConnLayer $ tr' fs

-- update filter

updateFullConnFilter :: FilterF -> Double -> [Maybe Layer] -> Layer
updateFullConnFilter fs lr dl
  | dl' == [] = FullConnLayer fs
  | otherwise = FullConnLayer fs'
  where
    dl' = catMaybes dl
    delta = mscale (lr / fromIntegral (length dl')) (msum $ strip dl')
    fs' = msub fs delta

strip :: [Layer] -> [FilterF]
strip [] = []
strip (FullConnLayer fs:ds) = fs:strip ds
strip (_:ds) = strip ds

