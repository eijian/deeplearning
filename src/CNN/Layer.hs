


module CNN.Layer where

import CNN.Image

type ActFunc = Double -> Double

relu :: ActFunc
relu a = if a > 0.0 then a else 0.0

data Layer = NopLayer
           | ActLayer ActFunc
           | MaxPoolLayer Int


forward :: Layer -> [Image] -> [Image]
forward NopLayer i = i
forward (ActLayer f) (i:is) = (map (map (map f)) i):i:is


