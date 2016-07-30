


module CNN.Layer where

import Debug.Trace
import CNN.LayerType

import CNN.Image
import CNN.ActLayer
import CNN.PoolLayer
import CNN.ConvLayer
import CNN.HiddenLayer
import CNN.FlattenLayer

forward :: Layer -> [Image] -> [Image]
forward _ [] = []
forward NopLayer i = i
forward l@(ActLayer f) im@(i:is)     = (activate l i):im
forward l@(MaxPoolLayer s) im@(i:is) = (poolMax l i):im
forward l@(ConvLayer s fs) im@(i:is) = (convolve s fs i):im
forward l@(HiddenLayer fs) im@(i:is) = (connect fs i):im
forward l@(FlattenLayer f) im@(i:is) = (f i):im


updateLayer :: [Layer] -> [[Layer]] -> [Layer]
updateLayer ls [] = ls
updateLayer ls (im:ims) = ls


