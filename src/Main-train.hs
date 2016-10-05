--
-- CNN : filter generator
--

module Main (
  main
) where

import Control.Monad
import Data.Time
import Debug.Trace

import CNN.Image
import CNN.Pool
import CNN.LayerType
import CNN.Layer
import CNN.ActLayer
import CNN.PoolLayer
import CNN.ConvLayer
import CNN.FullConnLayer

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
epochs = 200
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
  sampleE <- getImages poolE test_N 1

  fc1 <- initFilterC 10 1 12 12 3 2
  fc2 <- initFilterC 20 10 5 5 2 2
  ff1 <- initFilterF n_hidden (2*2*20)
  ff2 <- initFilterF n_out n_hidden

  let
    is = [epoch0 .. (epoch0 + epochs - 1)]
    layers = [
        ConvLayer 3 fc1
      , ActLayer relu
      , MaxPoolLayer 2
      , ConvLayer 2 fc2
      , ActLayer relu
      , MaxPoolLayer 2
      , FlattenLayer flatten unflatten 2 2
      , FullConnLayer ff1
      , ActLayer relu
      , FullConnLayer ff2
      , ActLayer softmax
      ]
    getTeachers = getImages poolT batch

  putStrLn "Training the model..."
  tm0 <- getCurrentTime
  putStatus tm0 0 [([0.0], 0.0)]
  loop getTeachers sampleE (putStatus tm0) layers is
  putStrLn "Finished!"

{- |
loop

  IN: func of getting teachers (including batch size and pool)
      sample of evaluation
      layers
      epoch numbers
      start time

-}

loop :: (Int -> IO [Trainer]) -> [Trainer]
     -> (Int -> [([Double], Double)] -> IO ()) -> [Layer] -> [Int]
     -> IO ()
loop _ _ _ _ [] = putStr ""
loop getT se putF ls (i:is) = do
  teachers <- getT i
  let
    rls = tail $ map reverseLayer $ reverse ls  -- fist element isn't used
    dls = reverse $ map (train ls rls) teachers
    ls' = update dls ls           -- dls = diff of layers
  if i `mod` opStep == 0
    then putF i (evaluate ls' se)
    else putStr ""
  loop getT se putF ls' is

putStatus :: UTCTime -> Int -> [([Double], Double)] -> IO ()
putStatus tm0 i rs = do
  tm <- getCurrentTime
  let
    (rv, rr) = unzip rs
  putStrLn (
    "iter = " ++ show (epoch0 + i - 1) ++ "/" ++ show (epoch0 + epochs - 1) ++
    " time = " ++ (show $ diffUTCTime tm tm0) ++
    " ratio = " ++ show (sum rr / fromIntegral (length rr)))
  --mapM_ putOne rs  
  where
    putOne :: ([Double], Double) -> IO ()
    putOne (v, r) = putStrLn ("result:" ++ show v ++ ", ratio:" ++ show r)



