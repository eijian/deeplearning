--
-- Trainer : train supervised dataset
--

module Trainer (
  train
, update
, evaluate
, forwardProp
) where

import Control.Monad
import Debug.Trace

import CNN.Algebra
import CNN.Image
import CNN.Layer
import CNN.LayerType

{-
train

  IN : layers
       reversed layers (for backward prop)
       trainig data

  OUT: difference of layers

-}

train :: [Layer] -> [Layer] -> Trainer -> [Layer]
train [] _ (i, c) = []
train ls rls (i, c) = dls
  where
    (y, op') = splitAt 1 $ forwardProp ls [i]
    d = [[head (head $ head y) `vsub` c]]
    (_, dls) = backwardProp (zip (tail op') rls) (d, [])

{-
update

  IN : learning rate
       differences of layers of each training data
       original layers

  OUT: updated layers

-}

update :: Double -> [Layer] -> [[Layer]] -> [Layer]
update lr [] _  = []
update lr ls [] = ls
update lr (l:ls) (dl:dls) = updateLayer lr l dl : update lr ls dls

{-
evaluate

  IN : layers
       trainer data

  OUT: result (output)
       ratio of correct answer

-}

evaluate :: [Layer] -> [Trainer] -> [([Double], Double)]
evaluate _ [] = []
evaluate ls (s:ss) = (op, rt) : evaluate ls ss
  where
    op = head $ head (head $ forwardProp ls [fst s])
    rt = sum $ zipWith (*) (snd s) op

---

{-
fowardProp

  IN : layers

  OUT: image list
       list of output of each layer

-}

forwardProp :: [Layer] -> [Image] -> [Image]
forwardProp [] is = is
forwardProp (l:ls) (i:is) = forwardProp ls (forwardLayer l i ++ is)

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
backwardProp ((im,l):ols) (d, ls) = backwardProp ols (d', l':ls)
  where
    (d', l') = backwardLayer l im d

{-
selectLayer :: Layer -> Bool
selectLayer (ConvLayer _ _)   = False
selectLayer (FullConnLayer _) = True
selectLayer _                 = False
-}

