

module CNN.LayerType where

import CNN.Image

-- for Activation Layer
type ActFunc = [Double] -> [Double]

-- filter for Convolution Layer
type Kernel = [[Double]]
type Bias   = Double
type FilterC = (Kernel, Bias)

-- filter for Hidden Layer
type FilterH = [Double]

data Layer = NopLayer
           | ActLayer ActFunc
           | MaxPoolLayer Int
           | ConvLayer Int [FilterC]
           | HiddenLayer [FilterH]
           | FlattenLayer (Image -> Image)


