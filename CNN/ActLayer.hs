--
-- ActLayer: activate layer
--

module CNN.ActLayer (
  relu
, softmax
, activate
) where

import CNN.LayerType
import CNN.Image

relu :: ActFunc
relu as = map f as
  where
    f :: Double -> Double
    f a = if a > 0.0 then a else 0.0

softmax :: ActFunc
softmax as = map (\x -> x / sume) es
  where
    amax = maximum as
    es   = map (\x -> exp (x - amax)) as
    sume = sum es

activate :: Layer -> Image -> Image
activate (ActLayer f) im = map (map f) im


