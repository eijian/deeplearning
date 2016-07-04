

module Main where

import Control.Monad
--import CNN.Cnn
import CNN.Image
import CNN.Pool
import CNN.Layer

-- PARAMETERS
k = 3
n = 50
m = 10

train_N    = n * k
test_N     = m * k
image_size = (12, 12)
channel    = 1

n_kernels    = [10, 20]
kernel_sizes = [(3, 3), (2, 2)]
pool_sizes   = [(2, 2), (2, 2)]
n_hidden     = 20

n_out = k

epochs = 500
opStep = 5
epoch0 = 1
--batch  = 150
batch  = 35

learning_rate = 0.1


-- MAIN

main :: IO ()
main = do
  sampleT <- initSamplePool 1 (12, 12) 3 0.95 150
  sampleE <- initSamplePool 1 (12, 12) 3 0.90 30
  let layers = NopLayer
  forM_ [epoch0 .. (epoch0 + epochs - 1)] $ \i -> do
    let ns  = nSample sampleT
        off = (i-1)*batch `mod` ns
    teachers <- getImages sampleT off batch
    let (im, cl) = unzip teachers
    ops <- mapM (learn layers) im
    if i `mod` opStep == 0 then putStatus i ops else putStr ""

learn :: Layer l => l -> Image -> IO [Image]
learn l i = do
  let op = forward l [i]
  return op 

putStatus :: Int -> [[Image]] -> IO ()
putStatus i ims = do
  putStrLn ("iter " ++ show (epoch0 + i - 1) ++ "/" ++ show (epoch0 + epochs - 1))


  
