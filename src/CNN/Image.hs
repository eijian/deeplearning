

module CNN.Image where


data Image = Image [Word8] Int
type ImageSize = (Int, Int)



createData :: Int -> Int -> ImageSize -> Int -> Double -> [Image]
createData n c sz k p =


reso :: ImageSize -> Int
reso (x, y) = x * y

iadd :: ImageSize -> ImageSize -> ImageSize
iadd (ax, ay) (bx, by) = (ax + bx, ay + by)

isub :: ImageSize -> ImageSize -> ImageSize
isub (ax, ay) (bx, by) = (ax - bx, ay - by)
