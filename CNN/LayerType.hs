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
--type Delta = [Double]
type Delta = Image

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

instance Eq Layer where
  NopLayer             == NopLayer             = True
  (ActLayer f1)        == (ActLayer f2)        = True
  (MaxPoolLayer s1)    == (MaxPoolLayer s2)    = s1 == s2
  (ConvLayer s1 fs1)   == (ConvLayer s2 fs2)   = s1 == s2 && fs1 == fs2
  (FullConnLayer fs1)  == (FullConnLayer fs2)  = fs1 == fs2
  (FlattenLayer x1 y1) == (FlattenLayer x2 y2) = x1 == x2 && y1 == y2
  _                    == _                    = False





