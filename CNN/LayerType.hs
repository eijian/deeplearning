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
, LayerB
) where

import Numeric.LinearAlgebra.Data

import CNN.Image

-- for Activation Layer
type ActFunc = Matrix R -> Matrix R

-- filter for Convolution Layer
type Kernel = Matrix R
type Bias   = Double
type FilterC = (Kernel, Bias)

-- filter for Fully Connected Layer
type FilterF = Matrix R

-- for backward propergation
--type Delta = [Double]
type Delta = Image

-- LayerB is for converting from parsed data to Layer
type LayerB = (String, [Int])

data Layer =
    NopLayer
  | ActLayer ActFunc
  | MaxPoolLayer Int
  | ConvLayer Int [FilterC]
  | FullConnLayer FilterF
  | FlattenLayer Int Int

instance Eq Layer where
  NopLayer             == NopLayer             = True
  (ActLayer f1)        == (ActLayer f2)        = True
  (MaxPoolLayer s1)    == (MaxPoolLayer s2)    = s1 == s2
  (ConvLayer s1 fs1)   == (ConvLayer s2 fs2)   = s1 == s2 && fs1 == fs2
  (FullConnLayer fs1)  == (FullConnLayer fs2)  = fs1 == fs2
  (FlattenLayer x1 y1) == (FlattenLayer x2 y2) = x1 == x2 && y1 == y2
  _                    == _                    = False

