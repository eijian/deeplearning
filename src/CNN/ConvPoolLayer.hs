

module CNN.ConvPoolLayer where


import CNN.Layer


-- CPLayer
--   n_kernel, channel
--   image_size, kernel_size, pool_size
--   activation
data CPLayer = CPLayer Int Int ImageSize ImageSize ImageSize Actfunc

imageSize :: CPLayer -> ImageSize
imageSize (CPLayer _ _ is _ _ _) = is

kernelSize :: CPLayer -> ImageSize
kernelSize (CPLayer _ _ _ ks _ _) = ks


initFilter :: CPLayer -> IO Filter
initFilter (CPLayer k c _ ksz psz) = do
  let f_in  = c * reso ksz
      f_out = k * reso ksz `div` reso psz
      a     = sqrt (6.0 / (f_in + f_out))
  return $ mkFilter k c (reso ksz) a


convolve :: CPLayer -> Int -> [Image] -> [Image]
convolve layer btsz ipimg =
  where
    minibatch_size = if length ipimg < btsz then length ipimg else btsz

    is = imageSize layer
    ks = kernelSize layer
    s  = (is `isub` ks) `iadd` (1, 1)

    
