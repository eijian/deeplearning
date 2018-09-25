--
-- Status : neural network status
--

module Status (
  loadStatus
, saveStatus
, Status
, layers
, learnR
, nclass
, batchSz
, testSz
, ntrained
, repeatCt
, savePt
, poolT
, poolE
) where

import Control.Exception
import Control.Monad

import CNN.ActLayer
import CNN.ConvLayer
import CNN.FullConnLayer
import CNN.Layer
import CNN.LayerType
import CNN.PoolLayer

import Pool

data Status = Status {
    dirname  :: String
  , layers   :: [Layer]
  , learnR   :: Double
  , nclass   :: Int
  , batchSz  :: Int
  , testSz   :: Int
  , ntrained :: Int
  , repeatCt :: Int
  , savePt   :: Int
  , poolT    :: MemPool
  , poolE    :: MemPool
  }

--
-- CONSTANTS
--

filterDir :: String
filterDir = "/filters/"

filterExt :: String
filterExt = ".filter"

--
-- FUNCTIONS
--

{-
loadStatus

  IN : filename

  OUT: loaded/built status

-}

loadStatus :: String -> IO Status
loadStatus name
  | name == "" = staticStatus
  | otherwise  = loadFromFile name

staticStatus :: IO Status
staticStatus = do
  let
    k = 3   -- number of class
    n = 50  -- number of teacher data for each class
    m = 10  -- number of test data for each class

    train_N    = n * k
    test_N     = m * k
    image_size = [12, 12]
    channel    = 1

    n_kernels    = [10, 20]
    kernel_sizes = [3, 2]
    pool_sizes   = [2, 2]
    n_hidden     = 20
    n_out = k

  pt <- initSamplePool 1 (12, 12) 3 0.95 (train_N * 10)
  pe <- initSamplePool 1 (12, 12) 3 0.90 (test_N * 10)

  fc1 <- initFilterC 10 1 12 12 3 2
  fc2 <- initFilterC 20 10 5 5 2 2
  ff1 <- initFilterF n_hidden (2*2*20)
  --ff2 <- initFilterF n_out n_hidden
  ff2 <- zeroFilterF n_out n_hidden

  let
    ls = [
        ConvLayer 3 fc1
      , ActLayer relu
      , MaxPoolLayer 2
      , ConvLayer 2 fc2
      , ActLayer relu
      , MaxPoolLayer 2
      , FlattenLayer 2 2
      , FullConnLayer ff1
      , ActLayer relu
      , FullConnLayer ff2
      , ActLayer softmax
      ]

    stat = Status {
        dirname  = ""
      , layers   = ls
      , learnR   = 0.1
      , nclass   = 3
      , batchSz  = 50
      , testSz   = 10
      , ntrained = 0
--      , repeatCt = 500
      , repeatCt = 100
      , savePt   = 5
      , poolT    = pt
      , poolE    = pe
      }

  return stat

loadFromFile :: String -> IO Status
loadFromFile dname = do
  let
    k = 3   -- number of class
    n = 50  -- number of teacher data for each class
    m = 10  -- number of test
    train_N    = n * k
    test_N     = m * k
    image_size = [12, 12]
    channel    = 1

    n_kernels    = [10, 20]
    kernel_sizes = [3, 2]
    pool_sizes   = [2, 2]
    n_hidden     = 20
    n_out = k

  pt <- initSamplePool 1 (12, 12) 3 0.95 (train_N * 10)
  pe <- initSamplePool 1 (12, 12) 3 0.90 (test_N * 10)

  fc1 <- initFilterC 10 1 12 12 3 2
  fc2 <- initFilterC 20 10 5 5 2 2
  ff1 <- initFilterF n_hidden (2*2*20)
  --ff2 <- initFilterF n_out n_hidden
  ff2 <- zeroFilterF n_out n_hidden

  let
    ls = [
        ConvLayer 3 fc1
      , ActLayer relu
      , MaxPoolLayer 2
      , ConvLayer 2 fc2
      , ActLayer relu
      , MaxPoolLayer 2
      , FlattenLayer 2 2
      , FullConnLayer ff1
      , ActLayer relu
      , FullConnLayer ff2
      , ActLayer softmax
      ]
    dir = dname ++ filterDir

  fs <- forM [0..(length ls - 1)] $ \i -> readFilter dir i

  let
    ls' = zipWith readLayer ls fs
    stat = Status {
        dirname  = dname
      , layers   = ls'
      , learnR   = 0.1
      , nclass   = 3
      , batchSz  = 50
      , testSz   = 10
      , ntrained = 0
--      , repeatCt = 500
      , repeatCt = 100
--      , savePt   = 5
      , savePt   = 10
      , poolT    = pt
      , poolE    = pe
      }

  return stat


{-
saveStatus

  IN : directoryname for output
       status
       updated layers

-}

saveStatus :: Status -> [Layer] -> IO ()
saveStatus st ls = do
  --putStrLn ("saved to '" ++ (dirname st) ++ "'")
  let
    dir = (dirname st) ++ filterDir
  zipWithM_ (writeFilter dir) [0..] ls

{-
readFilter
-}
readFilter :: String -> Int -> IO String
readFilter dir i = do
  let fn = dir ++ show i ++ filterExt
  -- putStrLn $ (show i ++ " reading...")
  res <- try $ readFile fn :: IO (Either SomeException String)
  case res of
    Left  _ -> return ""
    Right c -> return c

{-
writeFilter
-}
writeFilter :: String -> Int -> Layer -> IO ()
writeFilter dir i layer = do
  let fn = dir ++ show i ++ filterExt
  res <- try $ writeFile fn $ showFilter layer :: IO (Either SomeException ())
  case res of
    Left  _ -> putStrLn ("failed to write: " ++ fn)
    Right _ -> return ()
