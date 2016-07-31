

module Main where

import Control.Monad
import Data.Time
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
epochs = 10
opStep = 5
epoch0 = 1
batch  = 150
--batch  = 10

learning_rate = 0.1

-- MAIN

main :: IO ()
main = do
  putStrLn "Building the model..."
  poolT <- initSamplePool 1 (12, 12) 3 0.95 train_N
  poolE <- initSamplePool 1 (12, 12) 3 0.90 test_N
  sampleE <- getImages poolE 1 test_N

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

  putStrLn "Training the model..."
  putStatus 0 [([0.0], 0.0)] layers
  loop is layers batch poolT sampleE

  putStrLn "Finished!"

{- |
loop

  IN: epoch numbers
      layers
      batch size
      pool of teacher data
      sample of evaluation

-}

loop :: [Int] -> [Layer] -> Int -> MemPool -> [(Image, Class)] -> IO ()
loop [] _ _ _ _ = putStr ""
loop (i:is) ls b pt se = do
  teachers <- getImages pt i b
  ops <- mapM (train ls) teachers
  let (_, dls) = unzip ops
      ls' = updateLayer ls dls   -- dls = diff of layers
  if i `mod` opStep == 0 then putStatus i (evaluate ls' se) ls'
                         else putStr ""
  loop is ls' b pt se

putStatus :: Int -> [([Double], Double)] -> [Layer] -> IO ()
putStatus i rs ls = do
  let (_, rt) = unzip rs
  tm <- getCurrentTime
  putStrLn ("iter = " ++ show (epoch0 + i - 1) ++
            "/"     ++ show (epoch0 + epochs - 1) ++
            " time = " ++ (formatTime defaultTimeLocale "%H:%M:%S" tm) ++
            " ratio = " ++ show (sum rt / fromIntegral (length rt)))
  --mapM_ (putOne) rs
  where
    putOne :: ([Double], Double) -> IO ()
    putOne (v, r) = putStrLn ("result:" ++ show v ++ ", ratio:" ++ show r)



