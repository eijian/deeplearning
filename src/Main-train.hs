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
import Data.List (foldl')
import Debug.Trace
import Numeric.LinearAlgebra
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
import CNN.Propagation

import Pool
import Status

usage :: String
usage = "Usage: train <dir> [<image>]"

-- MAIN

main :: IO ()
main = do
  as <- getArgs
  case length as of
    1 -> train (as !! 0)
    2 -> judge (as !! 0) (as !! 1)
    _ -> error usage

train :: String -> IO ()
train dn = do
  putStr "Initializing..."
  st <- loadStatus dn
--  st <- loadStatus ""
  tm0 <- getCurrentTime

  let
    --getTeachers = getImages (poolT st) (batchSz st)
    --getTests    = getImages (poolE st) (testSz st)
    getTeachers = getImagesRandomly (poolT st) (batchSz st)
    getTests    = getImagesRandomly (poolE st) (testSz st)
    putF = putStatus tm0 st getTests
    loopFunc = trainLoop' getTeachers putF st
  putStrLn ("done. (trained: " ++ (show $ counter st) ++ " images)")
  putStrLn ("Training the model... (#batch:" ++ (show (batchSz st * nclass st)) ++ ")")
  putF 0 (layers st)
  layers' <- foldM' loopFunc (layers st) [1 .. (repeatCt st)]

  putStrLn "Saving status..."
  saveStatus st layers' (repeatCt st * batchSz st)
  putStrLn "Finished!"

judge :: String -> String -> IO ()
judge dn imf = do
  st <- loadStatus dn
  fh <- openFile imf ReadMode
  lst <- hGetContents fh
  forM_ (lines lst) $ \i -> do
    im <- datLoader i
    putStr (i ++ ",")
    let (y, _) = judgeImage (layers st) im
    forM_ (zip [0..] (toList y)) $ \(c, v) -> do
      let res = show c ++ (printf ":%.2f," (v*100))
      putStr res
    putStrLn ""

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

trainLoop' :: (Int -> IO [Trainer]) -> (Int -> [Layer] -> IO ()) -> Status
           -> [Layer] -> Int -> IO [Layer]
trainLoop' getT putF st ls i = do
  --teachers <- getT ((i-1) * batchSz st)
  teachers <- getT 0
  let ls' = updateLayers (learnR st) teachers ls
  if i `mod` (savePt st) == 0
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
    dls = map (trainLayers ls rls) ts             -- dls = diff of layers

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
  --tests <- getE ((i-1) * testSz st)
  tests <- getE 0
  let
    --(rv, rr) = unzip $ evaluate ls tests
    rr = foldl' (evaluateLayers ls) 0.0 tests
  let
    ite = printf "iter = %5d/%d " i (repeatCt st)
    acc = printf "accuracy = %.10f " (rr / fromIntegral (length tests))
  putStr (ite ++ acc)
  tm <- getCurrentTime
  putStrLn ("time = " ++ show (diffUTCTime tm tm0))
  saveStatus st ls i
  --mapM_ putOne rs   -- for debug
  where
    putOne :: ([Double], Double) -> IO ()
    putOne (v, r) = putStrLn ("result:" ++ show v ++ ", ratio:" ++ show r)
