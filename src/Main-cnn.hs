

module Main where

import Control.Monad
--import CNN.Cnn
import CNN.Image
import CNN.Pool
import CNN.LayerType
import CNN.Layer
import CNN.ActLayer
import CNN.PoolLayer

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
opStep = 5
epoch0 = 1
--batch  = 150
batch  = 1

learning_rate = 0.1


-- MAIN

main :: IO ()
main = do
  sampleT <- initSamplePool 1 (12, 12) 3 0.95 150
  sampleE <- initSamplePool 1 (12, 12) 3 0.90 30

  let layers = [NopLayer, ActLayer relu, MaxPoolLayer 2]
      nsams  = nSample sampleT

  forM_ [epoch0 .. (epoch0 + epochs - 1)] $ \i -> do
    let off = (i-1) * batch `mod` nsams
    teachers <- getImages sampleT off batch
    ops <- mapM (learn layers) teachers
    if i `mod` opStep == 0 then putStatus i ops else putStr ""

learn :: [Layer] -> (Image, Class) -> IO [Image]
learn [] (i, c) = do
  return [i]
learn (l:ls) i = do
  i' <- learn ls i
  return $ forward l i'

putStatus :: Int -> [[Image]] -> IO ()
putStatus i ims = do
  putStrLn ("iter " ++ show (epoch0 + i - 1) ++ "/" ++ show (epoch0 + epochs - 1))
  putStrLn $ show ims

  
