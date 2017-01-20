--
-- Layer : layer definition
--

module CNN.Layer (
  forwardLayer
, backwardLayer
, reverseLayer
, updateLayer
) where

import Debug.Trace


import CNN.ActLayer
import CNN.ConvLayer
import CNN.FlattenLayer
import CNN.FullConnLayer
import CNN.Image
import CNN.LayerType
import CNN.PoolLayer

forwardLayer :: Layer -> Image -> [Image]
forwardLayer (NopLayer)         i = [i]
forwardLayer (ActLayer f)       i = [activate f    i, i]
forwardLayer (MaxPoolLayer s)   i =  poolMax  s    i
forwardLayer (ConvLayer s fs)   i = [convolve s fs i, i]
forwardLayer (FullConnLayer fs) i = [connect  fs   i, i]
forwardLayer (FlattenLayer _ _) i = [flatten       i, i]

backwardLayer :: Layer -> Image -> Delta -> (Delta, Maybe Layer)
backwardLayer (NopLayer)         _  d = (d, Nothing)
backwardLayer (ActLayer f)       im d = deactivate f    im d
backwardLayer (MaxPoolLayer s)   im d = depoolMax  s    im d
backwardLayer (ConvLayer s fs)   im d = deconvolve s fs im d
backwardLayer (FullConnLayer fs) im d = deconnect    fs im d
backwardLayer (FlattenLayer x y) im d = (unflatten x y d, Nothing)

reverseLayer :: Layer -> Layer
reverseLayer (ActLayer f)         = reverseActFunc f
reverseLayer (MaxPoolLayer s)     = reversePooling s
reverseLayer (ConvLayer s fs)     = reverseConvFilter s fs
reverseLayer (FullConnLayer fs)   = reverseFullConnFilter fs
reverseLayer (NopLayer)           = NopLayer
reverseLayer l@(FlattenLayer _ _) = l

updateLayer :: Double -> Layer -> [Maybe Layer] -> Layer
updateLayer lr (ConvLayer s fs)   dl = updateConvFilter s fs lr dl
updateLayer lr (FullConnLayer fs) dl = updateFullConnFilter fs lr dl
updateLayer _  l                  _  = l

