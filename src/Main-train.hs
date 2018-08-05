--
-- CNN : CNN filter generator
--
{-# LANGUAGE BangPatterns #-}

module Main (
  main
) where

import Control.DeepSeq
import Control.Monad
--import Data.List
import Data.Time
import Debug.Trace
import System.Environment
import System.IO
import Text.Printf

import CNN.ActLayer
import CNN.Algebra
import CNN.ConvLayer
import CNN.FullConnLayer
import CNN.Image
import CNN.Layer
import CNN.LayerType
import CNN.PoolLayer

import Pool
import Status
import Trainer

usage :: String
usage = "Usage: train <dir>"

-- MAIN

main :: IO ()
main = do
  as <- getArgs
  dn <- if length as == 1
    then return (as !! 0)
    else error usage
  putStrLn "Initializing..."
  st <- loadStatus dn
  tm0 <- getCurrentTime

  let
    is = [1 .. (repeatCt st)]
    getTeachers = getImages (poolT st) (batchSz st)
    getTests    = getImages (poolE st) (ntest st)
    putF = putStatus tm0 st getTests
    loopFunc = trainLoop' getTeachers putF (learnR st) (savePt st)

  putStrLn "Training the model..."
  putF 0 (layers st)
  layers' <- foldM' loopFunc (layers st) is

  putStrLn "Saving status..."
  saveStatus st layers'
  putStrLn "Finished!"

--
-- FUNCTIONS
--

{- |
foldM' : monadic version of foldl'

  URL: http://stackoverflow.com/questions/8919026/does-haskell-have-foldlm

-}

foldM' :: (Monad m) => ([a] -> b -> m [a]) -> [a] -> [b] -> m [a]
foldM' _ z [] = return z
foldM' f z (x:xs) = do
  z' <- f z x
  z' `seq` foldM' f z' xs

{- |
trainLoop

  IN : func of getting teachers (including batch size and pool)
       sample of evaluation
       output func
       learning rate
       layers
       epoch numbers

  OUT: updated layers

-}

trainLoop' :: (Int -> IO [Trainer]) -> (Int -> [Layer] -> IO ()) -> Double
           -> Int -> [Layer] -> Int -> IO [Layer]
trainLoop' getT putF lr savep ls i = do
  teachers <- getT i
  let ls' = updateLayers lr teachers ls
  if i `mod` savep == 0
    then putF i ls'
    else return ()
  return ls'

{- |
updateLayers
-}

updateLayers :: Double -> [Trainer] -> [Layer] -> [Layer]
updateLayers lr ts ls = update lr ls (transpose dls)
  where
    rls = tail $ map reverseLayer $ reverse ls    -- fist element isn't used
    dls = map (train ls rls) ts             -- dls = diff of layers

{-
putStatus

  IN : start time
       status
       start epoch number
       end epoch number
       step size of status output
       epoch
       layers
       sample of evaluation

-}

putStatus :: UTCTime -> Status -> (Int -> IO [Trainer]) -> Int -> [Layer] -> IO ()
putStatus tm0 st getE i ls = do
  tests <- getE i
  let
    (rv, rr) = unzip $ evaluate ls tests
    ite = printf "iter = %5d/%d " i (repeatCt st)
    acc = printf "accuracy = %.10f " (sum rr / fromIntegral (length rr))
  putStr (ite ++ acc)
  tm <- getCurrentTime
  putStrLn ("time = " ++ show (diffUTCTime tm tm0))
  saveStatus st ls
  --mapM_ putOne rs   -- for debug
  where
    putOne :: ([Double], Double) -> IO ()
    putOne (v, r) = putStrLn ("result:" ++ show v ++ ", ratio:" ++ show r)
