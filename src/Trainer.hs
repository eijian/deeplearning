--
-- Trainer : train supervised dataset
--

module Trainer (
  train
, update
, evaluate
) where

import Control.Monad
import Debug.Trace

import CNN.Image
import CNN.LayerType
import CNN.Layer
import CNN.Algebra

train :: [Layer] -> [Layer] -> Trainer -> [Layer]
train [] _ (i, c) = []
train ls rls (i, c) = ds
  where
    (y, op') = splitAt 1 $ forwardProp ls [i]
    d = (head $ head $ head y) `vsub` c
    (_, ds) = backwardProp (zip (tail op') rls) (d, [])

update :: Double -> [[Layer]] -> [Layer] -> [Layer]
update lr _ [] = []
update lr [] ls = ls
--update lr (dl:dls) (l:ls) = trace ("DL=" ++ show dl ++ "/L=" ++ show l) $ (updateLayer lr l dl):(update lr dls ls)
update lr (dl:dls) (l:ls) = (updateLayer lr l dl):(update lr dls ls)

evaluate :: [Layer] -> [Trainer] -> [([Double], Double)]
evaluate _ [] = []
evaluate ls (s:ss) = (op, rt):(evaluate ls ss)
  where
    op = head $ head (head $ forwardProp ls [fst s])
    rt = sum $ zipWith (*) (snd s) op

---
forwardProp :: [Layer] -> [Image] -> [Image]
forwardProp [] is = is
forwardProp (l:ls) im@(i:is) = forwardProp ls ((forwardLayer l i):im)

{- |
backwardProp

  IN : list of image and layer pair
       delta from previous step
       difference of layers
  OUT: delta
       difference of layers

-}

backwardProp :: [(Image, Layer)] -> (Delta, [Layer]) -> (Delta, [Layer])
backwardProp [] (_, ls) = ([], ls)
--backwardProp ((im,l):ols) (d, ls) = trace ("D=" ++ show d ++ "/D'=" ++ show d') $ backwardProp ols (d', l':ls)
backwardProp ((im,l):ols) (d, ls) = backwardProp ols (d', l':ls)
  where
    (d', l') = backwardLayer l im d

selectLayer :: Layer -> Bool
selectLayer (ConvLayer _ _)   = False
selectLayer (FullConnLayer _) = True
selectLayer _                 = False
