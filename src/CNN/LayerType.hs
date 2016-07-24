

module CNN.LayerType where

-- for Activation Layer
type ActFunc = Double -> Double

-- for Convolution Layer
type Kernel = [[Double]]
type Bias   = Double
type Filter = (Kernel, Bias)

data Layer = NopLayer
           | ActLayer ActFunc
           | MaxPoolLayer Int
           | ConvLayer Int [Filter]

