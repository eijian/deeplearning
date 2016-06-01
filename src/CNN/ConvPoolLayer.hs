

module CNN.ConvPoolLayer where


import CNN.Layer


-- CPLayer
--   n_kernel, channel
--   image_size, kernel_size, pool_size
--   activation
data CPLayer = CPLayer Int Int ImageSize ImageSize ImageSize Actfunc



initFilter :: CPLayer -> IO Filter
initFilter (CPLayer k c _ ksz psz) = do
  let f_in  = c * reso ksz
      f_out = k * reso ksz `div` reso psz
      a     = sqrt (6.0 / (f_in + f_out))
  return $ mkFilter k c (reso ksz) a


