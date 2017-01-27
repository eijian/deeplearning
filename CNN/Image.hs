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
import Numeric.LinearAlgebra.Data

type Plain = Matrix R    -- 2D: X x Y pixels
type Image = [Plain]     -- n dimension

type Class = Vector R    -- teacher vector

type Trainer = (Image, Class)

