--
-- test 
--

module Main (
  main
) where

import Test.DocTest

main :: IO ()
main = doctest [
    "CNN/ActLayer.hs"
  , "CNN/Algebra.hs"
  , "CNN/ConvLayer.hs"
  , "CNN/FlattenLayer.hs"
  , "CNN/FullConnLayer.hs"
  , "CNN/Image.hs"
  , "CNN/Layer.hs"
  , "CNN/LayerType.hs"
  , "CNN/PoolLayer.hs"
  ]
