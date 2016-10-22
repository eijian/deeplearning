--
-- Status : neural network status
--

module Status (
  loadStatus
, layers
, learnR
, batch
, epoch0
, nepoch
, opstep
, poolT
, poolE
, ntest
) where

import CNN.LayerType
import CNN.ActLayer
import CNN.PoolLayer
import CNN.ConvLayer
import CNN.FullConnLayer

import Pool

data Status = Status {
    layers :: [Layer]
  , learnR :: Double
  , batch  :: Int
  , epoch0 :: Int
  , nepoch :: Int
  , opstep :: Int
  , poolT  :: MemPool
  , poolE  :: MemPool
  , ntest  :: Int
  }

loadStatus :: String -> IO Status
loadStatus name
  | name == "" = staticStatus
  | otherwise  = loadFromFile name

staticStatus :: IO Status
staticStatus = do
  let
    -- data size
    k = 3
    n = 50
    m = 10
    -- kernel 1
    ksize1 = 3
    nchannel1 = 1
    nkernel1 = 10
    -- kernel 2
    ksize2 = 2
    nchannel2 = nkernel1
    nkernel2 = 20
    -- pooling layer
    psize1 = 2
    psize2 = 2
    -- image size
    xsize1 = 12
    ysize1 = 12
    xsize2 = (xsize1 - ksize1 + 1) `div` psize1
    ysize2 = (ysize1 - ksize1 + 1) `div` psize1
    xsize3 = (xsize2 - ksize2 + 1) `div` psize2
    ysize3 = (ysize2 - ksize2 + 1) `div` psize2

    nhidden = 20
    nout = k

  pt <- initSamplePool 1 (xsize1, ysize1) ksize1 0.95 (n * k)
  pe <- initSamplePool 1 (xsize1, ysize1) ksize2 0.95 (m * k)

  fc1 <- initFilterC nkernel1 nchannel1 xsize1 ysize1 ksize1 psize1
  fc2 <- initFilterC nkernel2 nchannel2 xsize2 ysize2 ksize2 psize2
  ff1 <- initFilterF nhidden (psize2 * psize2 * nhidden)
  --ff2 <- initFilterF nout nhidden
  ff2 <- zeroFilterF nout nhidden

  let
    ls = [
        ConvLayer ksize1 fc1
      , ActLayer relu
      , MaxPoolLayer psize1
      , ConvLayer ksize2 fc2
      , ActLayer relu
      , MaxPoolLayer psize2
      , FlattenLayer xsize3 ysize3 
      , FullConnLayer ff1
      , ActLayer relu
      , FullConnLayer ff2
      , ActLayer softmax
      ]
    stat = Status {
        layers = ls
      , learnR = 0.1
      , batch  = 150
      , epoch0 = 1
      , nepoch = 500
      , opstep = 5
      , poolT  = pt
      , poolE  = pe
      , ntest  = m * k
      }
  return stat

loadFromFile :: String -> IO Status
loadFromFile fname = staticStatus


  