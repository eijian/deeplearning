--
-- Status : neural network status
--

module Status (
  loadStatus
, saveStatus
, Status
, layers
, learnR
, batchSz
, ntrained
, repeatCt
, savePt
, poolT
, poolE
, ntest
) where

import CNN.ActLayer
import CNN.ConvLayer
import CNN.FullConnLayer
import CNN.LayerType
import CNN.PoolLayer

import Pool

data Status = Status {
    layers   :: [Layer]
  , learnR   :: Double
  , batchSz  :: Int
  , ntrained :: Int
  , repeatCt :: Int
  , savePt   :: Int
  , poolT    :: MemPool
  , poolE    :: MemPool
  , ntest    :: Int
  }

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
        layers   = ls
      , learnR   = 0.1
      , batchSz  = 150
      , ntrained = 0
      , repeatCt = 500
      , savePt   = 5
      , poolT    = pt
      , poolE    = pe
      , ntest    = m * k
      }

  return stat

loadFromFile :: String -> IO Status
loadFromFile fname = staticStatus

{-
saveStatus

  IN : filename for output
       status
       updated layers

-}

saveStatus :: String -> Status -> [Layer] -> IO ()
saveStatus fname st ls = putStrLn ("saved to '" ++ fname ++ "'")
