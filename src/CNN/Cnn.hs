

module CNN.Cnn where



data Cnn 


train :: Cnn -> Int -> Double -> [Image] -> [Image] -> IO ()
train cnn epo lrate train test =
  forM_ [1..epo] $ \i -> do
    putStrLn ("epoch: " + show i)

