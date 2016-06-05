


module CNN.ConvLayer where

import Control.Monad
import Data.List
import qualified Data.Map as M
import Data.Maybe
import System.Random.Mersenne as MT

import CNN.Image

data ConvLayer = CL {
    channel :: Int
  , kSize   :: Int
  , ipSize  :: (Int, Int)
  , nKernel :: Int
  , scanPx  :: [Int]
  , offset  :: [Int]
  } deriving (Eq, Read, Show)

type Kernel = [[Double]]
type Bias   = Double
type Filter = (Kernel, Bias)

initConvLayer :: Int -> Int -> Int -> Int -> Int -> ConvLayer
initConvLayer ch x y nk ksz = CL ch ksz (x, y) nk sc off
  where
    sc  = sort $ [i + x * j | i <- [0..(x-ksz)], j <- [0..(y-ksz)]]
    off = sort $ [n + x * m | n <- [0..(ksz-1)], m <- [0..(ksz-1)]]

initFilters :: ConvLayer -> IO [Filter]
initFilters (CL c k (x, y) n _ _) = do
  let a = sqrt (3.0 / (fromIntegral x * fromIntegral y))
  f <- forM [1..n] $ \i -> do
    w <- initKernel c k a
    return (w, 0.0)
  return f
  where
    initKernel :: Int -> Int -> Double -> IO Kernel
    initKernel c k a = do
      let npx = k * k
      w <- forM [1..npx] $ \i -> do
        w' <- forM [1..c] $ \j -> do
          r <- MT.randomIO :: IO Double
          return ((r * 2.0 - 1.0) * a)
        return w'
      return w

--

forward :: ConvLayer -> [Filter] -> Image -> Image
forward cl f im = M.fromList $ zip [0..(length im - 1)] im'
  where
    im' = map (convolve f im (offset cl)) (scanPx cl)

convolve :: [Filter] -> Image -> [Int] -> Int -> Pixel
convolve f im o s = convolvePx f px
  where
    px = mapMaybe (\x -> M.lookup x im) $ map (s +) o

-- loop of n kernel
convolvePx :: [(Kernel, Bias)] -> [Pixel] -> Pixel
convolvePx [] _ = []
convolvePx (wb:wbs) ps = (a:convolvePx wbs ps)
  where
    w = fst wb
    b = snd wb
    a = dot w ps + b

dot :: Kernel -> [Pixel] -> Double
dot [] _ = 0.0
dot _ [] = 0.0
dot (k:ks) (p:ps) = (dot' k p) + dot ks ps
  where
    dot' :: [Double] -> Pixel -> Double
    dot' k p = sum $ zipWith (*) k p

