

module CNN.Image (
  Image
, Plain
, Class
, flatten
, unflatten  
) where

import qualified Data.Map as M

{-
data Image = Image {
    imgPixel :: [[Word8]]
  , imgLabel :: Int
  } deriving (Eq, Read, Show)
type ImageSize = (Int, Int)



createData :: Int -> Int -> ImageSize -> Int -> Double -> [Image]
createData n c sz k p =


reso :: ImageSize -> Int
reso (x, y) = x * y

iadd :: ImageSize -> ImageSize -> ImageSize
iadd (ax, ay) (bx, by) = (ax + bx, ay + by)

isub :: ImageSize -> ImageSize -> ImageSize
isub (ax, ay) (bx, by) = (ax - bx, ay - by)

-}

{-
type Plain = [Double]
type Image = M.Map Int Plain    -- (X * Y)

loadImage :: String -> Int -> IO Image
loadImage f c = do
  hdl <- openFile f ReadMode
  dtmp <- BS.hGetContents
  let dt = map (fromIntegral) B.unpack dtmp :: [Double]
      
      im  = M.fromList $ zip [0..(length data - 1)] im'
  return im

split :: Int -> [Double] -> [Pixel]
split c dt = 
-}

type Plain = [[Double]]    -- 2D: X x Y pixels
type Image = [Plain]     -- n dimension

type Class = [Double]    -- teacher vector

{- |
flattenImage

  IN: Image (=[[[Double]]])

>>> let im = [[[2.0,3.0],[4.0,5.0]],[[1.0,2.0],[4.0,3.0]],[[2.0,1.0],[6.0,5.0]]]
>>> flatten im
[[[2.0,3.0,4.0,5.0,1.0,2.0,4.0,3.0,2.0,1.0,6.0,5.0]]]

-}

flatten :: Image -> Image
flatten im = [[concat $ concat im]]

{- |
unflattenImage

  IN: [[[Double]]]

>>> let ds = [[[2.0,3.0,4.0,5.0,1.0,2.0,4.0,3.0,2.0,1.0,6.0,5.0]]]
>>> unflatten 2 2 ds
[[[2.0,3.0],[4.0,5.0]],[[1.0,2.0],[4.0,3.0]],[[2.0,1.0],[6.0,5.0]]]

-}

unflatten :: Int -> Int -> Image -> Image
unflatten x y [[ds]] = split y $ split x ds
  where
    split :: Int -> [a] -> [[a]]
    split _ [] = []
    split n ds = (d:split n ds')
      where
        (d, ds') = splitAt n ds
