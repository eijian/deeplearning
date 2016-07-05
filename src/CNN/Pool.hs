

module CNN.Pool where

import Control.Monad
import Data.Maybe
import qualified Data.Map as M
import System.Random.Mersenne as MT

import CNN.Image

type MemPool = M.Map Int (Image, Class)

{-
  channel
  image size
  output size (#class)
  probablity
  n samples
-}

initSamplePool :: Int -> (Int, Int) -> Int -> Double -> Int -> IO MemPool
initSamplePool c (sx, sy) o p n = do
  s0 <- forM [0..(n-1)] $ \i -> do
    s1 <- forM [0..(c-1)] $ \j -> do
      s2 <- forM [0..(sy-1)] $ \y -> do
        let p' = if y `div` st == i `mod` o then p else (1-p)
        s3 <- forM [0..(sx-1)] $ \x -> do
          a <- pixel p'
          return a
        return s3
      return s2
    e1 <- forM [0..(o-1)] $ \j -> do
      return $ if i `mod` o == j then 1.0 else 0.0
    return (s1, e1)
  return $ M.fromList $ zip [0..(n-1)] s0
  where
    st = sy `div` o

pixel :: Double -> IO Double
pixel p = do
  v <- MT.randomIO :: IO Double
  let v' = if v < p then 0.5 else 0.0
  return v'

{-
  pool
  offset
  #image
-}

getImages :: MemPool -> Int -> Int -> IO [(Image, Class)]
getImages p o n = do
  let im0 = mapMaybe (\x -> M.lookup x p) [o..mx1]
      im1 = mapMaybe (\x -> M.lookup x p) [0..mx2]
  --putStrLn (show psize ++ "/" ++ show o ++ "/" ++ show mx1 ++ "/" ++ show mn2 ++ "/" ++ show mx2)
  return (im0 ++ im1)
  where
    psize = nSample p
    mx0 = o + n - 1
    mx2 = mx0 - psize
    mx1 = if mx2 < 0 then mx0 else psize - 1
  
nSample :: MemPool -> Int
nSample p = M.size p


