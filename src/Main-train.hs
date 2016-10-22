--
-- CNN : filter generator
--

module Main (
  main
) where

import Control.Monad
import Data.Time
import Debug.Trace

import CNN.Algebra
import CNN.Image
import CNN.LayerType
import CNN.Layer
import CNN.ActLayer
import CNN.PoolLayer
import CNN.ConvLayer
import CNN.FullConnLayer

import Pool
import Status
import Trainer

{-
-- PARAMETERS
k = 3   -- number of class
n = 50  -- number of teacher data for each class
m = 10  -- number of test data for each class

train_N    = n * k
test_N     = m * k
image_size = (12, 12)
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
-}

-- MAIN

main :: IO ()
main = do
  st <- loadStatus ""
  putStrLn "Building the model..."
  sampleE <- getImages (poolE st) (ntest st) 1
  tm0 <- getCurrentTime

  let
    is = reverse $ [(epoch0 st) .. ((epoch0 st) + (nepoch st) - 1)]
    getTeachers = getImages (poolT st) (batch st)
    putF = putStatus tm0 (nepoch st) (epoch0 st) (opstep st)

  x <- getTeachers 1
  putStrLn "Training the model..."
  putF 0 (layers st) sampleE
  layers' <- loop getTeachers sampleE putF (learnR st) (layers st) is
  -- saveStatus st layers'
  putStrLn "Finished!"

{- |
loop

  IN: func of getting teachers (including batch size and pool)
      sample of evaluation
      output func
      learning rate
      layers
      epoch numbers
  OUT:updated layers
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
