
module Main where

import CNN.Kernel

main :: IO ()
main = do
  f <- mkFilter 10 3 2 2 0.4
  putFilter f

putFilter :: Filter -> IO ()
putFilter (ks, b) = do
  putStrLn ("Bias: " ++ show b)
  putKernel ks
  
putKernel :: [Kernel] -> IO ()
putKernel []     = putStr ""
putKernel (k:ks) = do
  putStrLn $ show $ length k
  putKernelData k
  putKernel ks

putKernelData :: Kernel -> IO ()
putKernelData []     = putStrLn "/kernel"
putKernelData (l:ls) = do
  putStrLn $ show l
  putKernelData ls
