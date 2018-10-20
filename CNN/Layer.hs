--
-- Layer : layer definition
--

module CNN.Layer (
  forwardLayer
, backwardLayer
, readLayer
, reverseLayer
, showFilter
, updateLayer
, convertLayerB
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

showFilter :: Layer -> String
showFilter (ActLayer _)       = ""
showFilter (MaxPoolLayer _)   = ""
showFilter (ConvLayer _ fs)   = show fs
showFilter (FullConnLayer fs) = show fs
showFilter (NopLayer)         = ""
showFilter (FlattenLayer _ _) = ""

{-
readLayer

-}

readLayer :: Layer -> String -> Layer
readLayer l@(ConvLayer s _)   f =
  if f /= "" then ConvLayer s (read f :: [FilterC])
             else l
readLayer l@(FullConnLayer _) f =
  if f /= "" then FullConnLayer (read f :: FilterF)
             else l
readLayer l _                   = l

{- |

-}

convertLayerB :: LayerB -> IO Layer
convertLayerB (ln, para) = do
  case ln of
    "convolution"   -> do
      -- initFilterC co ci x y kn pl
      fc <- initFilterC (para!!3) (para!!2) (para!!0) (para!!1) (para!!4) (para!!5)
      return $ ConvLayer (para!!4) fc
    "activation"    -> return $ ActLayer $ numToFunc (para!!0)
    "pooling"       -> return $ MaxPoolLayer (para!!0)
    "flatten"       -> return $ FlattenLayer (para!!0) (para!!1)
    "fullconnected" -> do
      fc <- if para!!2 == 0
        then zeroFilterF (para!!1) (para!!0)
        else initFilterF (para!!1) (para!!0)
      return $ FullConnLayer fc
    _               -> error ("no such layer name: " ++ ln)




