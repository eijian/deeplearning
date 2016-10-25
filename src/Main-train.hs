--
-- CNN : filter generator
--

module Main (
  main
) where

import Control.Monad
import Data.Time
import Debug.Trace

import CNN.ActLayer
import CNN.Algebra
import CNN.ConvLayer
import CNN.FullConnLayer
import CNN.Image
import CNN.Layer
import CNN.LayerType
import CNN.PoolLayer

import Pool
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

epochs = 500
--epochs = 2000
--epochs = 1
opStep = 5
--opStep = 1
epoch0 = 1
batch  = 150
--batch  = 1

learning_rate = 0.1

-- MAIN

main :: IO ()
main = do
  putStrLn "Building the model..."
  poolT <- initSamplePool 1 (12, 12) 3 0.95 train_N
  poolE <- initSamplePool 1 (12, 12) 3 0.90 test_N
  sampleE <- getImages poolE test_N 1
  tm0 <- getCurrentTime

  fc1 <- initFilterC 10 1 12 12 3 2
  fc2 <- initFilterC 20 10 5 5 2 2
  ff1 <- initFilterF n_hidden (2*2*20)
  --ff2 <- initFilterF n_out n_hidden
  ff2 <- zeroFilterF n_out n_hidden

  let
    is = reverse $ [epoch0 .. (epoch0 + epochs - 1)]
    getTeachers = getImages poolT batch
    putF = putStatus tm0 epochs epoch0 opStep
    layers = [
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

  putStrLn "Training the model..."
  putF 0 layers sampleE
  layers' <- loop getTeachers sampleE putF learning_rate layers is
  putStrLn "Finished!"

{- |
loop

  IN : func of getting teachers (including batch size and pool)
       sample of evaluation
       output func
       learning rate
       layers
       epoch numbers

  OUT: updated layers

-}

loop :: (Int -> IO [Trainer]) -> [Trainer]
     -> (Int -> [Layer] -> [Trainer] -> IO ())
     -> Double -> [Layer] -> [Int] -> IO [Layer]
loop _ _ _ _ ls [] = return ls
loop getT se putF lr ls (i:is) = do
  ls' <- loop getT se putF lr ls is
  teachers <- getT i
  let
    rls = tail $ map reverseLayer $ reverse ls'    -- fist element isn't used
    dls = map (train ls' rls) teachers
    ls'' = update lr (transpose dls) ls'           -- dls = diff of layers
  putF i ls'' se
  return ls''

{-
putStatus

  IN : start time
       number of epoch
       start epoch number
       step size of status output
       epoch
       layers
       sample of evaluation

-}

putStatus :: UTCTime -> Int -> Int -> Int -> Int -> [Layer] -> [Trainer]
          -> IO ()
putStatus tm0 ep ep0 step i ls se
  | i `mod` step /= 0 = putStr ""
  | otherwise         = do
    let
      (rv, rr) = unzip $ evaluate ls se
    tm <- getCurrentTime
    putStrLn (
      "iter = " ++ show (ep0 + i - 1) ++ "/" ++ show (ep0 + ep - 1) ++
      " time = " ++ (show $ diffUTCTime tm tm0) ++
      " ratio = " ++ show (sum rr / fromIntegral (length rr)))
    --mapM_ putOne rs  
    where
      putOne :: ([Double], Double) -> IO ()
      putOne (v, r) = putStrLn ("result:" ++ show v ++ ", ratio:" ++ show r)
