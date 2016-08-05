
module Main where

import Control.Monad
import qualified Data.Map as M

import CNN.ConvLayer
import CNN.Image

imgdat :: [Pixel]
imgdat = [[200.0], [200.0], [200.0], [200.0], [200.0], [200.0],
          [200.0], [200.0], [200.0], [200.0], [200.0], [200.0],
          [200.0], [200.0], [200.0], [200.0], [200.0], [200.0],
          [200.0], [200.0], [200.0], [200.0], [200.0], [200.0],
          [200.0], [200.0], [200.0], [200.0], [200.0], [200.0],
          [200.0], [200.0], [200.0], [200.0], [200.0], [200.0],
          [200.0], [200.0], [200.0], [200.0], [200.0], [200.0],
          [200.0], [200.0], [200.0], [200.0], [200.0], [200.0],
          [0.0], [0.0], [0.0], [0.0], [0.0], [0.0],
          [0.0], [0.0], [0.0], [0.0], [0.0], [0.0],
          [0.0], [0.0], [0.0], [0.0], [0.0], [0.0],
          [0.0], [0.0], [0.0], [0.0], [0.0], [0.0],
          [0.0], [0.0], [0.0], [0.0], [0.0], [0.0],
          [0.0], [0.0], [0.0], [0.0], [0.0], [0.0],
          [0.0], [0.0], [0.0], [0.0], [0.0], [0.0],
          [0.0], [0.0], [0.0], [0.0], [0.0], [0.0],
          [100.0], [100.0], [100.0], [100.0], [100.0], [100.0],
          [100.0], [100.0], [100.0], [100.0], [100.0], [100.0],
          [100.0], [100.0], [100.0], [100.0], [100.0], [100.0],
          [100.0], [100.0], [100.0], [100.0], [100.0], [100.0],
          [100.0], [100.0], [100.0], [100.0], [100.0], [100.0],
          [100.0], [100.0], [100.0], [100.0], [100.0], [100.0],
          [100.0], [100.0], [100.0], [100.0], [100.0], [100.0],
          [100.0], [100.0], [100.0], [100.0], [100.0], [100.0]]

img = M.fromList $ zip [0..(length imgdat - 1)] imgdat


main :: IO ()
main = do
  let cl = initConvLayer 1 12 12 10 3
  fl    <- initFilters cl
  putStrLn ("CL:" ++ show cl)
  forM_ fl $ \i -> do
    putFilter i
  putStrLn ("IM(I):" ++ show img)
  let opImage = forward cl fl img
  putStrLn ("IM(O):" ++ show opImage)


putFilter :: Filter -> IO ()
putFilter (w, b) = do
  putStrLn ("B:" ++ show b)
  putStrLn ("W:" ++ show w)

