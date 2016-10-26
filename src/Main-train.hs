--
-- CNN : CNN filter generator
--

module Main (
  main
) where

import Control.Monad
import Data.Time
import Debug.Trace

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
  st <- loadStatus ""
  sampleE <- getImages (poolE st) (ntest st) 1
  tm0 <- getCurrentTime

  let
    --is = reverse $ [(epoch0 st) .. ((epoch0 st) + (nepoch st) - 1)]
    is = [(epoch_ed st), (epoch_ed st)-1 .. (epoch_st st)]
    getTeachers = getImages (poolT st) (batch st)
    putF = putStatus tm0 (epoch_st st) (epoch_ed st) (opstep st)

  putStrLn "Training the model..."
  putF 0 (layers st) sampleE
  layers' <- trainLoop getTeachers sampleE putF (learnR st) (layers st) is
  saveStatus "" st layers'
  putStrLn "Finished!"

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
    ls'' = update lr (transpose dls) ls'           -- dls = diff of layers
  putF i ls'' se
  return ls''

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
    tm <- getCurrentTime
    putStrLn (
      "iter = " ++ show (eps + i - 1) ++ "/" ++ show epe ++
      " time = " ++ (show $ diffUTCTime tm tm0) ++
      " ratio = " ++ show (sum rr / fromIntegral (length rr)))
    --mapM_ putOne rs  
    where
      putOne :: ([Double], Double) -> IO ()
      putOne (v, r) = putStrLn ("result:" ++ show v ++ ", ratio:" ++ show r)

