--
-- Layer : layer definition
--

module CNN.Layer (
  forwardProp
, backwardProp
, reverseLayers
, updateLayer
) where

import Debug.Trace
import CNN.LayerType

import CNN.Image
import CNN.ActLayer
import CNN.PoolLayer
import CNN.ConvLayer
import CNN.FullConnLayer
--import CNN.FlattenLayer

forwardProp :: [Layer] -> [Image] -> [Image]
forwardProp [] is = is
forwardProp (l:ls) is = forwardProp ls (forward l is)

forward :: Layer -> [Image] -> [Image]
forward _ [] = []
forward l im@(i:_) = (forward' l i):im
  where
    forward' :: Layer -> Image -> Image
    forward' NopLayer i = []
    forward' (ActLayer f)            i = activate f    i
    forward' (MaxPoolLayer s)        i = poolMax  s    i
    forward' (ConvLayer s fs)        i = convolve s fs i
    forward' (FullConnLayer fs)      i = connect  fs   i
    forward' (FlattenLayer ff _ _ _) i = ff       i

backwardProp :: [(Image, Layer)] -> (Delta, [Layer]) -> (Delta, [Layer])
backwardProp [] (_, ls) = ([], ls)
backwardProp ((im,l):ols) dl = backwardProp ols (backward l im dl)

backward :: Layer -> Image -> (Delta, [Layer]) -> (Delta, [Layer])
backward l im (d, ls) = (d', (l':ls))
  where
    (d', l') = backward' l im d
    backward' :: Layer -> Image -> Delta -> (Delta, Layer)
    backward' (NopLayer)       _  d = (d, NopLayer)
    backward' (ActLayer f)     im d = deactivate f    im d
    backward' (MaxPoolLayer s) im d = depoolMax  s    im d
    backward' (ConvLayer s fs) im d = deconvolve s fs im d
    backward' (FullConnLayer fs) im d = deconnect    fs im d
    backward' l@(FlattenLayer _ uf x y) im d = (head $ head $ unflatten x y [[d]], l)

reverseLayers :: [Layer] -> [Layer]
reverseLayers ls = map reversel $ reverse ls
  where
    reversel :: Layer -> Layer
    reversel l@(NopLayer)              = l
    reversel l@(ActLayer f)            = reverseActFunc f
    reversel l@(MaxPoolLayer s)        = reversePooling s
    reversel l@(ConvLayer s fs)        = reverseConvFilter s fs
    reversel l@(FullConnLayer fs)      = reverseFullConnFilter fs
    reversel l@(FlattenLayer f uf x y) = l

updateLayer :: [Layer] -> [[Layer]] -> [Layer]
updateLayer ls [] = ls
updateLayer ls dls = ls
  where
    tls = transLayer dls
    diff = foldl mergeLayers dl dls

transLayer :: [[Layer]] -> [[Layer]]
transLayer ls
  | a == [] = []
  | otherwise = a:(transLayer ls')
  where
    (as, ls') = tr ls
    a = concat as
    tr :: [[Layer]] -> ([[Layer]], [[Layer]])
    tr ls = unzip $ map (splitAt 1) ls

mergeLayers :: Layer -> Layer -> Layer
mergeLayers (ConvLayer s1 fs1) (ConvLayer s2 fs2) = ConvLayer s1 fs1
mergeLayers (FullConnLayer fs1) (FullConnLayer fs1) = FullConnLayer fs1
mergeLayers x _ = x


