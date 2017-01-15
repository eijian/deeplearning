--
-- CNN : CNN filter generator
--
--{-# LANGUAGE BangPatterns #-}

module Main (
  main
) where

import Control.Monad
--import Data.List
import Data.Time
import Debug.Trace
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

-- MAIN

main :: IO ()
main = do
  putStrLn "Initializing..."
  st <- loadStatus ""
  sampleE <- getImages (poolE st) (ntest st) 1
  tm0 <- getCurrentTime

  let
    --is = [(epoch_ed st), (epoch_ed st - 1) .. (epoch_st st)]
    is = [(epoch_st st) .. (epoch_ed st)]
    getTeachers = getImages (poolT st) (batch st)
    putF = putStatus tm0 (epoch_st st) (epoch_ed st) (opstep st)
    loopFunc = trainLoop' getTeachers sampleE putF (learnR st)

  putStrLn "Training the model..."
  putF 0 (layers st) sampleE
  --layers' <- trainLoop getTeachers sampleE putF (learnR st) (layers st) is
  layers' <- foldM loopFunc (layers st) is

  putStrLn "Saving status..."
  saveStatus "" st layers'
  putStrLn "Finished!"

--
-- FUNCTIONS
--

{- |
foldM' : monadic version of foldl'

  URL: http://stackoverflow.com/questions/8919026/does-haskell-have-foldlm

-}

foldM' :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
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

{-
trainLoop :: (Int -> IO [Trainer]) -> [Trainer]
  -> (Int -> [Layer] -> [Trainer] -> IO ()) -> Double -> [Layer] -> [Int]
  -> IO [Layer]
trainLoop _ _ _ _ ls []             = return ls
trainLoop getT se putF lr ls (i:is) = do
  ls' <- trainLoop getT se putF lr ls is
  teachers <- getT i
  let
    rls = tail $ map reverseLayer $ reverse ls'    -- fist element isn't used
    dls = map (train ls' rls) teachers
    ls'' = update lr ls' (transpose dls)     -- dls = diff of layers
  putF i ls'' se
  return ls''
-}

trainLoop' :: (Int -> IO [Trainer]) -> [Trainer]
  -> (Int -> [Layer] -> [Trainer] -> IO ()) -> Double -> [Layer] -> Int
  -> IO [Layer]
trainLoop' getT se putF lr ls i = do
  teachers <- getT i
  let ls' = updateLayers lr teachers ls
  putF i ls' se
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
       start epoch number
       end epoch number
       step size of status output
       epoch
       layers
       sample of evaluation

-}

putStatus :: UTCTime -> Int -> Int -> Int -> Int -> [Layer] -> [Trainer]
          -> IO ()
putStatus tm0 eps epe step i ls se
  | i `mod` step /= 0 = putStr ""
  | otherwise         = do
    let
      (rv, rr) = unzip $ evaluate ls se
      ite = printf "iter = %5d/%d " (eps + i - 1) epe
      acc = printf "accuracy = %.10f " (sum rr / fromIntegral (length rr))
    putStr (ite ++ acc)
    tm <- getCurrentTime
    putStrLn ("time = " ++ show (diffUTCTime tm tm0))
    --mapM_ putOne rs  
    where
      putOne :: ([Double], Double) -> IO ()
      putOne (v, r) = putStrLn ("result:" ++ show v ++ ", ratio:" ++ show r)

