

module Main where

import Control.Monad
import Debug.Trace

import CNN.Image
import CNN.Pool
import CNN.LayerType
import CNN.Layer
import CNN.ActLayer
--import CNN.PoolLayer
import CNN.ConvLayer
import CNN.HiddenLayer

import Trainer

-- PARAMETERS
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

--epochs = 500
epochs = 1
opStep = 1
epoch0 = 1
batch  = 150
--batch  = 10

learning_rate = 0.1

-- MAIN

main :: IO ()
main = do
  sampleT <- initSamplePool 1 (12, 12) 3 0.95 150
  sampleE <- initSamplePool 1 (12, 12) 3 0.90 30

  fc1 <- initFilterC 10 1 12 12 3 2
  fc2 <- initFilterC 20 10 5 5 2 2
  fh1 <- initFilterH n_hidden 800
  fh2 <- initFilterH n_out n_hidden

  let layers = [ConvLayer 3 fc1, ActLayer relu, MaxPoolLayer 2,
                ConvLayer 2 fc2, ActLayer relu, MaxPoolLayer 2,
                FlattenLayer flatten,
                HiddenLayer fh1, ActLayer relu,
                HiddenLayer fh2, ActLayer softmax]
      is = [epoch0 .. (epoch0 + epochs - 1)]

  loop is layers batch sampleT


loop :: [Int] -> [Layer] -> Int -> MemPool -> IO ()
loop [] _ _ _ = putStrLn "finished!"
loop (i:is) ls b p = do
  teachers <- getImages p i b
  ops <- mapM (train ls) teachers
  let (output, dls) = unzip ops
      ls' = updateLayer ls dls   -- dls = diff of layers
  if i `mod` opStep == 0 then putStatus i output ls' else putStr ""
  loop is ls' b p

putStatus :: Int -> [Image] -> [Layer] -> IO ()
putStatus i ims ls = do
  putStrLn ("iter " ++ show (epoch0 + i - 1) ++
                "/" ++ show (epoch0 + epochs - 1))
  mapM_ (putOne) ims
  where
    putOne :: Image -> IO ()
    putOne im = putStrLn $ show im


