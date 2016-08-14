

module Trainer where

import Control.Monad
--import CNN.Cnn
import CNN.Image
import CNN.Pool
import CNN.LayerType
import CNN.Layer
import CNN.ActLayer
import CNN.PoolLayer
import CNN.ConvLayer
import CNN.HiddenLayer



train :: [Layer] -> Teacher -> IO (Image, [Layer])
train [] (i, c) = return (i, [])
train ls (i, c) = do
  let op  = forwardProp ls [i]
      ls' = backwordProp ls op
  return (head op, ls')



evaluate :: [Layer] -> [Teacher] -> [([Double], Double)]
evaluate _ [] = []
evaluate ls (s:ss) = (op, rt):(evaluate ls ss)
  where
    op = head $ head (head $ forwardProp ls [fst s])
    rt = sum $ zipWith (*) (snd s) op

forwardProp :: [Layer] -> [Image] -> [Image]
forwardProp [] is = is
forwardProp (l:ls) is = forwardProp ls (forward l is)


backwordProp :: [Layer] -> [Image] -> [Layer]
backwordProp [] _ = []
backwordProp ls [] = ls
backwordProp ls op = ls


