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
, fileExtDat
, fileExtGray
, fileExtColor
, datLoader
, pnmLoader
) where

import           Control.Exception
import           Control.Monad
import           Data.List
import qualified Data.ByteString        as B
import qualified Data.Map               as Map
import           Data.Maybe
import qualified Data.Vector            as V
import           Data.Word
import           Debug.Trace
import qualified Numeric.LinearAlgebra  as NL
import           System.Directory
import qualified System.IO.Strict       as SIO
import qualified System.Random.Mersenne as MT

import CNN.Image

--
-- CONSTANTS
--

fileExtDat   = ".dat" :: String
fileExtGray  = ".pgm" :: String
fileExtColor = ".ppm" :: String

tableWord8toDouble :: NL.Vector NL.R
tableWord8toDouble = NL.fromList [x / 256.0 | x <- [0..255]]

--
-- TYPES
--

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
type ImageLoader = String -> IO Image
data Pool =
  MemPool {
    m :: Map.Map Int Trainer
  , c :: Int
  } |
  FilePool {
    imageFiles :: [V.Vector ImageFile]
  , numclass   :: Int
  , classvec   :: [Class]
  , loader     :: ImageLoader
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
  getImages (FilePool flist nc cv ld) bt ofs = do
    let flist' = selectPerClass flist bt ofs
    ts <- forM flist' $ \(c, fn) -> do
      im <- ld fn
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
  getImagesRandomly p@(FilePool flist nc _ _) bt s = do
    r0 <- MT.randomIO :: IO Double
    let
      r = if r0 == 1.0 then 0.0 else r0
      s = nSample p `div` nc
      ofs = floor (r * (fromIntegral s))
    getImages p bt ofs

  nSample (MemPool m _) = Map.size m
  nSample (FilePool flist _ _ _) = count flist
    where
      count :: [V.Vector ImageFile] -> Int
      count [] = 0
      count (x:xs) = length x + count xs

  nClass (MemPool _ c)  = c
  nClass (FilePool _ c _ _) = c

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
      return $ NL.fromLists w
    -- Trainer data
    e1 <- forM [0..(o-1)] $ \j ->
      return $ if j == cl then 1.0 else 0.0
    return (s1, NL.fromList e1)
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

initFilePool :: String -> String -> Int -> ImageLoader -> IO Pool
initFilePool path ext nc ld = do
  let classv = map (classNumToVec nc) [0..(nc-1)]
  flist <- forM [0..(nc-1)] $ \i -> do
    let dir = path ++ "/" ++ (show i) ++ "/"
    --files <- listDirectory dir
    files <- getDirectoryContents dir
    let files' = map (makeImageFile i dir) $ filter (isSuffixOf ext) files
    return (V.fromList files')
  return (FilePool flist nc classv ld)
  where
    makeImageFile :: Int -> String -> String -> ImageFile
    makeImageFile i dir fn = (i, dir ++ fn)

{-
  loadImage
-}

datLoader :: String -> IO Image
datLoader fn = do
  res <- try $ (SIO.run $ SIO.readFile fn) :: IO (Either SomeException String)
  return $ case res of
    Left  e -> error ("read error:" ++ (show e))
    Right s -> read s :: Image

pnmLoader :: Int -> Int -> Int -> String -> IO Image
pnmLoader len x ch fn = do
  res <- try $ B.readFile fn :: IO (Either SomeException B.ByteString)
  return $ case res of
    Left  e -> error ("read error:" ++ (show e))
    Right s -> buildImage len x ch s

--
-- PRIVATE FUNCTIONS
--

{- |
  selectPerClass

>>> let fs = [V.fromList [(0,"a0"), (0,"a1"), (0,"a2"), (0,"a3"), (0,"a4"), (0,"a5"), (0,"a6"), (0,"a7"), (0,"a8"), (0,"a9")], V.fromList [(1,"b0"), (1,"b1"), (1,"b2"), (1,"b3"), (1,"b4"), (1,"b5"), (1,"b6"), (1,"b7"), (1,"b8"), (1,"b9"), (1,"b10")]]
>>> selectPerClass fs 3 0
[(0,"a0"),(0,"a1"),(0,"a2"),(1,"b0"),(1,"b1"),(1,"b2")]
>>> selectPerClass fs 3 8
[(0,"a8"),(0,"a9"),(0,"a0"),(1,"b8"),(1,"b9"),(1,"b10")]
>>> selectPerClass fs 3 10
[(0,"a0"),(0,"a1"),(0,"a2"),(1,"b10"),(1,"b0"),(1,"b1")]
>>> selectPerClass fs 15 12
[(0,"a2"),(0,"a3"),(0,"a4"),(0,"a5"),(0,"a6"),(0,"a7"),(0,"a8"),(0,"a9"),(0,"a0"),(0,"a1"),(0,"a2"),(0,"a3"),(0,"a4"),(0,"a5"),(0,"a6"),(1,"b1"),(1,"b2"),(1,"b3"),(1,"b4"),(1,"b5"),(1,"b6"),(1,"b7"),(1,"b8"),(1,"b9"),(1,"b10"),(1,"b0"),(1,"b1"),(1,"b2"),(1,"b3"),(1,"b4")]

-}

selectPerClass :: [V.Vector ImageFile] -> Int -> Int -> [ImageFile]
selectPerClass [] _ _ = []
selectPerClass (x:xs) bt ofs = V.toList fl ++ selectPerClass xs bt ofs
  where
    len = length x
    ofs' = ofs `mod` len
    ed = ofs' + bt - 1
    x1 = V.take bt $ V.drop ofs' x
    fl = if ed >= len then x1 V.++ V.take ((ed - (len-1)) `mod` len) x else x1

{- |
  buildImage

>>> let str = B.pack ([0..11] :: [Word8])
>>> buildImage 12 2 3 str
[(2><2)
 [        0.0, 1.171875e-2
 , 2.34375e-2, 3.515625e-2 ],(2><2)
 [  3.90625e-3,  1.5625e-2
 , 2.734375e-2, 3.90625e-2 ],(2><2)
 [ 7.8125e-3, 1.953125e-2
 ,  3.125e-2, 4.296875e-2 ]]
-}

buildImage :: Int -> Int -> Int -> B.ByteString -> Image
buildImage len x ch str = map (NL.reshape x) m
  where
    w8 = B.unpack $ B.drop (B.length str - len) str
    dat = map (\i -> tableWord8toDouble NL.! (fromIntegral i)) w8
    m = NL.toColumns $ NL.matrix ch dat
