--
-- LayerType : layer types
--

module CNN.LayerType (
  ActFunc
, Kernel
, Bias
, FilterC
, FilterF
, Delta
, Layer ( NopLayer, ActLayer, MaxPoolLayer, ConvLayer, FullConnLayer
        , FlattenLayer)
) where

import CNN.Image

-- for Activation Layer
type ActFunc = [Double] -> [Double]

-- filter for Convolution Layer
type Kernel = [[Double]]
type Bias   = Double
type FilterC = (Kernel, Bias)

-- filter for Fully Connected Layer
type FilterF = [Double]

-- for backward propergation
type Delta = [Double]

data Layer =
    NopLayer
  | ActLayer ActFunc
  | MaxPoolLayer Int
  | ConvLayer Int [FilterC]
  | FullConnLayer [FilterF]
  | FlattenLayer Int Int

instance Show Layer where
  show NopLayer           = "NopLayer"
  show (ActLayer f)       = "ActLayer"
  show (MaxPoolLayer s)   = "MaxPoolLayer:" ++ show s
  show (ConvLayer s fs)   = "ConvLayer:" ++ show s
  show (FullConnLayer fs) = "FullConnLayer:" ++ show fs
  show (FlattenLayer x y) = "FlattenLayer:" ++ show x ++ "/" ++ show y





