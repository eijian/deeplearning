--
-- Pool : image pool
--

module CNN.Pool (
  Pool
, MemPool
, getImages
, initSamplePool
) where

import           Control.Monad
import           Data.Maybe
import qualified Data.Map               as Map
import qualified System.Random.Mersenne as MT

import CNN.Image

class Pool p where
  {-
  getImages
    IN : pool
         batch size
         #epoch
  -}
  getImages :: p -> Int -> Int -> IO [Trainer]
  {-
  nSample
    IN : pool
  -}
  nSample :: p -> Int

---- IMAGE POOL ON MEMORY ----
------------------------------

newtype MemPool = MemPool { m :: Map.Map Int Trainer }

{-
initSamplePool
  IN : channel
       image size
       output size (#class)
       probablity
       n samples
-}

initSamplePool :: Int -> (Int, Int) -> Int -> Double -> Int -> IO MemPool
initSamplePool c (sx, sy) o p n = do
  s0 <- forM [0..(n-1)] $ \i -> do
    let
      cl = i `mod` o  -- class of this image

    -- Image data
    s1 <- forM [1..c] $ \j -> do
      s2 <- forM [0..(sy-1)] $ \y -> do
        let
          p' = if y `div` st == cl then p else (1-p)
        s3 <- forM [1..sx] $ \x -> do
          a <- pixel p'
          return a
        return s3
      return s2

    -- Trainer data
    e1 <- forM [0..(o-1)] $ \j -> do
      return $ if j == cl then 1.0 else 0.0
    return (s1, e1)

  return (MemPool (Map.fromList $ zip [0..] s0))
  where
    st = sy `div` o
    pixel :: Double -> IO Double
    pixel p = do
      v <- MT.randomIO :: IO Double
      let v' = if v < p then 0.5 else 0.0
      return v'

instance Pool MemPool where
  getImages p@(MemPool m) b e = do
    let
      s = nSample p
      o = (e-1) * b `mod` s
      mx0 = o + b - 1
      mx2 = mx0 - s
      mx1 = if mx2 < 0 then mx0 else s - 1
      im0 = mapMaybe (\x -> Map.lookup x m) [o..mx1]
      im1 = mapMaybe (\x -> Map.lookup x m) [0..mx2]
    return (im0 ++ im1)

  nSample (MemPool m) = Map.size m


---- IMAGE POOL FROM FILES ----
-------------------------------

