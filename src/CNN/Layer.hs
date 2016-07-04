


module CNN.Layer where

import CNN.Image

class Layer l where
  forward  :: l -> [Image] -> [Image]
  --backward :: 



data NopLayer = NopLayer

instance Layer NopLayer where
  forward l i = i
