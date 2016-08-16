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

train :: [Layer] -> Trainer -> (Image, [Layer])
train [] (i, c) = (i, [])
train ls (i, c) = (head op, ls')
  where
    op  = forwardProp ls [i]
    ls' = backwordProp ls op

evaluate :: [Layer] -> [Trainer] -> [([Double], Double)]
evaluate _ [] = []
evaluate ls (s:ss) = (op, rt):(evaluate ls ss)
  where
    op = head $ head (head $ forwardProp ls [fst s])
    rt = sum $ zipWith (*) (snd s) op

