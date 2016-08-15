--
-- Trainer : train supervised dataset
--

module Trainer (
  train
, evaluate
) where

import Control.Monad
import CNN.Image
import CNN.LayerType
import CNN.Layer

train :: [Layer] -> Trainer -> IO (Image, [Layer])
train [] (i, c) = return (i, [])
train ls (i, c) = do
  let op  = forwardProp ls [i]
      ls' = backwordProp ls op
  return (head op, ls')

evaluate :: [Layer] -> [Trainer] -> [([Double], Double)]
evaluate _ [] = []
evaluate ls (s:ss) = (op, rt):(evaluate ls ss)
  where
    op = head $ head (head $ forwardProp ls [fst s])
    rt = sum $ zipWith (*) (snd s) op

