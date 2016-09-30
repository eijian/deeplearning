--
-- Trainer : train supervised dataset
--

module Trainer (
  train
, evaluate
) where

import Control.Monad
import Debug.Trace

import CNN.Image
import CNN.LayerType
import CNN.Layer
import CNN.Algebra

train :: [Layer] -> [Layer] -> Trainer -> ([Double], [Layer])
train [] _ (i, c) = (head $ head i, [])
train ls (rl:rls) (i, c) = (y', ls')
  where
    op  = forwardProp ls [i]
    (y, op') = splitAt 1 op
    ols = zip (tail op') rls
    y' = head $ head $ head y
    (d, ls') = backwardProp ols (y' `vsub` c, [rl])

evaluate :: [Layer] -> [Trainer] -> [([Double], Double)]
evaluate _ [] = []
evaluate ls (s:ss) = (op, rt):(evaluate ls ss)
  where
    op = head $ head (head $ forwardProp ls [fst s])
    rt = sum $ zipWith (*) (snd s) op

