--
-- Image : image data structure
--

module CNN.Image (
  Image
, Plain
, Class
, Trainer
, classNumToVec
) where

import qualified Data.Map as M
import Numeric.LinearAlgebra.Data

type Plain = Matrix R    -- 2D: X x Y pixels
type Image = [Plain]     -- n dimension

type Class = Vector R    -- teacher vector

type Trainer = (Image, Class)

-- FUNCTIONS

classNumToVec :: Int -> Int -> Class
classNumToVec n c = fromList ls
  where
    ls = take n (replicate c 0.0 ++ [1.0] ++ repeat 0.0)
