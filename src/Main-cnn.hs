

module Main where

import CNN.Cnn

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
learning_rate = 0.1

main :: IO ()
main = do
  let train_data = createData n channel (fst image_size) k 0.95
      test_data  = createData m channel (fst image_size) k 0.90
      classifier = Cnn 

  train classifier epochs learning_rate train_data test_data
