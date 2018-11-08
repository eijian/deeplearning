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
import Data.List (foldl')
import Debug.Trace
import Numeric.LinearAlgebra

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

train :: [Layer] -> [Layer] -> Trainer -> [Maybe Layer]
train [] _ (i, c) = []
train ls rls (i, c) = dls
  where
--    (y, op') = splitAt 1 $ forwardProp ls [i]
    (y, op') = splitAt 1 $ foldl' forwardProp' [i] ls
    d = [reshape (size c) ((flatten $ head $ head y) `vsub` c)]
    (_, dls) = backwardProp (zip (tail op') rls) (d, [])

{-
update

  IN : learning rate
       differences of layers of each training data
       original layers

  OUT: updated layers

-}

update :: Double -> [Layer] -> [[Maybe Layer]] -> [Layer]
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

{- 過去の実装
evaluate :: [Layer] -> [Trainer] -> [(Vector R, Double)]
evaluate _ [] = []
evaluate ls (s:ss) = (op, rt) : evaluate ls ss
  where
    op = flatten $ head (head $ forwardProp ls [fst s])
    rt = (snd s) <.> op
-}

evaluate :: [Layer] -> Double -> Trainer -> Double
evaluate ls rr (i, c) = rr + c <.> op
  where
    -- op = flatten $ head (head $ forwardProp ls [i])
    is = foldl' forwardProp' [i] ls
    op = flatten $ head (head is)

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

forwardProp' :: [Image] -> Layer -> [Image]
forwardProp' (i:is) l = forwardLayer l i ++ is

{- |
backwardProp

  IN : list of image and layer pair
       delta from previous step
       difference of layers

  OUT: delta
       difference of layers

-}

backwardProp :: [(Image, Layer)] -> (Delta, [Maybe Layer])
             -> (Delta, [Maybe Layer])
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

