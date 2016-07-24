


module CNN.ActLayer where

--import {-# SOURCE #-} CNN.Layer
import CNN.LayerType
import CNN.Image

relu :: ActFunc
relu a = if a > 0.0 then a else 0.0

activate :: Layer -> Image -> Image
activate (ActLayer f) im = map (map (map f)) im


