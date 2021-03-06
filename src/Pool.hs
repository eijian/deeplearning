--
-- Pool : image pool
--

module Pool (
  PoolClass
, Pool
, getImages
, getImagesRandomly
, nSample
, nClass
, initSamplePool
, initFilePool
, loadImage
) where

import           Control.Exception
import           Control.Monad
import           Data.List
import qualified Data.Map               as Map
import           Data.Maybe
import           Debug.Trace
import           Numeric.LinearAlgebra
import           System.Directory
import qualified System.IO.Strict       as SIO
import qualified System.Random.Mersenne as MT

import CNN.Image

class PoolClass p where
  {-
  getImages
    IN : pool
         batch size
         offset
  -}
  getImages :: p -> Int -> Int -> IO [Trainer]
  getImagesRandomly :: p -> Int -> Int -> IO [Trainer]
  nSample :: p -> Int
  nClass :: p -> Int

type ImageFile = (Int, String)
data Pool =
  MemPool {
    m :: Map.Map Int Trainer
  , c :: Int
  } |
  FilePool {
    imageFiles :: [[ImageFile]]
  , numclass :: Int
  , classvec :: [Class]
  }

instance PoolClass Pool where
  getImages p@(MemPool m _) bt ofs = do
    let
      s = nSample p
      o = ofs `mod` s
      mx0 = o + bt - 1
      mx2 = mx0 - s
      mx1 = if mx2 < 0 then mx0 else s - 1
      im0 = mapMaybe (`Map.lookup` m) [o..mx1]
      im1 = mapMaybe (`Map.lookup` m) [0..mx2]
    return (im0 ++ im1)
  getImages (FilePool flist nc cv) bt ofs = do
    let flist' = selectPerClass flist bt ofs
    ts <- forM flist' $ \(c, fn) -> do
      im <- loadImage fn
      return $ Just (im, cv !! c)
    let trainers = catMaybes ts
    return trainers

  getImagesRandomly p@(MemPool m _) bt s = do
    r0 <- MT.randomIO :: IO Double
    let
      r = if r0 == 1.0 then 0.0 else r0
      s = nSample p
      ofs = floor (r * (fromIntegral s))
    getImages p bt ofs
  getImagesRandomly p@(FilePool flist nc _) bt s = do
    r0 <- MT.randomIO :: IO Double
    let
      r = if r0 == 1.0 then 0.0 else r0
      s = nSample p `div` nc
      ofs = floor (r * (fromIntegral s))
    getImages p bt ofs

  nSample (MemPool m _) = Map.size m
  nSample (FilePool flist _ _) = count flist
    where
      count :: [[ImageFile]] -> Int
      count [] = 0
      count (x:xs) = length x + count xs

  nClass (MemPool _ c)  = c
  nClass (FilePool _ c _) = c

---- IMAGE POOL ON MEMORY ----
------------------------------
{-
initSamplePool
  IN : channel
       image size
       output size (#class)
       probablity
       n samples

  OUT: memory pool
-}

initSamplePool :: Int -> (Int, Int) -> Int -> Double -> Int -> IO Pool
initSamplePool c (sx, sy) o p n = do
  s0 <- forM [0..(n-1)] $ \i -> do
    let
      cl = i `mod` o  -- class of this image

    -- Image data
    s1 <- forM [1..c] $ \j -> do
      w <-forM [0..(sy-1)] $ \y -> do
        let
          p' = if y `div` st == cl then p else 1-p
        forM [1..sx] $ \x -> pixel p'
      return $ fromLists w
    -- Trainer data
    e1 <- forM [0..(o-1)] $ \j ->
      return $ if j == cl then 1.0 else 0.0
    return (s1, fromList e1)
  return (MemPool (Map.fromList $ zip [0..] s0) c)
  where
    st = sy `div` o
    pixel :: Double -> IO Double
    pixel p = do
      v <- MT.randomIO :: IO Double
      let v' = if v < p then 0.5 else 0.0
      return v'

---- IMAGE POOL FROM FILES ----
-------------------------------
{- |
  initFilePool
  IN : pool dir path
       number of class
  OUT:
-}

initFilePool :: String -> Int -> IO Pool
initFilePool path nc = do
  let classv = map (classNumToVec nc) [0..(nc-1)]
  flist <- forM [0..(nc-1)] $ \i -> do
    let dir = path ++ "/" ++ (show i) ++ "/"
    --files <- listDirectory dir
    files <- getDirectoryContents dir
    let files' = map (makeImageFile i dir) $ filter (isSuffixOf ".dat") files
    return files'
  return (FilePool flist nc classv)
  where
    makeImageFile :: Int -> String -> String -> ImageFile
    makeImageFile i dir fn = (i, dir ++ fn)

loadImage :: String -> IO Image
loadImage fn = do
  res <- try $ (SIO.run $ SIO.readFile fn) :: IO (Either SomeException String)
  return $ case res of
    Left  e -> error ("read error:" ++ (show e))
    Right s -> read s :: Image

--
-- PRIVATE FUNCTIONS
--

selectPerClass :: [[ImageFile]] -> Int -> Int -> [ImageFile]
selectPerClass [] _ _ = []
selectPerClass (x:xs) bt ofs = fl ++ (selectPerClass xs bt ofs)
  where
    len = length x
    ofs' = ofs `mod` len
    ed = ofs' + bt - 1
    ed' = if ed > len then (ed - len) `mod` len else ed
    range = if ed' >= ofs'
      then [ofs' .. ed']
      else [0..ed'] ++ [ofs'..(len - 1)]
    fl = map (x !!) range
