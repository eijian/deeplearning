--
-- Image : image data structure
--

module CNN.Image (
  Image
, Plain
, Class
, Trainer
) where

import qualified Data.Map as M

type Plain = [[Double]]    -- 2D: X x Y pixels
type Image = [Plain]     -- n dimension

type Class = [Double]    -- teacher vector

type Trainer = (Image, Class)

