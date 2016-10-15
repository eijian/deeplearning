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
import CNN.LayerType

import CNN.Image
import CNN.ActLayer
import CNN.PoolLayer
import CNN.ConvLayer
import CNN.FullConnLayer
import CNN.FlattenLayer

forwardLayer :: Layer -> Image -> Image
forwardLayer NopLayer i           = []
forwardLayer (ActLayer f)       i = activate f    i
forwardLayer (MaxPoolLayer s)   i = poolMax  s    i
forwardLayer (ConvLayer s fs)   i = convolve s fs i
forwardLayer (FullConnLayer fs) i = connect  fs   i
forwardLayer (FlattenLayer _ _) i = flatten       i

backwardLayer :: Layer -> Image -> Delta -> (Delta, Layer)
backwardLayer (NopLayer)       _  d = (d, NopLayer)
backwardLayer (ActLayer f)     im d = deactivate f    im d
backwardLayer (MaxPoolLayer s) im d = depoolMax  s    im d
backwardLayer (ConvLayer s fs) im d = deconvolve s fs im d
backwardLayer (FullConnLayer fs) im d = deconnect    fs im d
backwardLayer l@(FlattenLayer x y) im d = (head $ head $ unflatten x y [[d]], l)

reverseLayer :: Layer -> Layer
reverseLayer (ActLayer f)         = reverseActFunc f
reverseLayer (MaxPoolLayer s)     = reversePooling s
reverseLayer (ConvLayer s fs)     = reverseConvFilter s fs
reverseLayer (FullConnLayer fs)   = reverseFullConnFilter fs
reverseLayer (NopLayer)           = NopLayer
reverseLayer l@(FlattenLayer _ _) = l

updateLayer :: Double -> Layer -> [Layer] -> Layer
updateLayer lr (ConvLayer s fs)   dl = updateConvFilter s fs lr dl
updateLayer lr (FullConnLayer fs) dl = updateFullConnFilter fs lr dl
updateLayer lr l dl = l

{-
transLayer :: [[Layer]] -> [[Layer]]
transLayer ls
  | a == [] = []
  | otherwise = a:(transLayer ls')
  where
    (as, ls') = tr ls
    a = concat as
--    a = as
    tr :: [[Layer]] -> ([[Layer]], [[Layer]])
    tr ls = unzip $ map (splitAt 1) ls
-}

mergeLayers :: Layer -> Layer -> Layer
mergeLayers (ConvLayer s1 fs1) (ConvLayer s2 fs2) = ConvLayer s1 fs1
mergeLayers (FullConnLayer fs1) (FullConnLayer fs2) = FullConnLayer fs1
mergeLayers x _ = x


