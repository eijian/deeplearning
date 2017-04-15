--
-- Config : read configuration
--

module Config (
) where

import Data.Char
import Control.Applicative hiding (many)
import Text.Parsec hiding ((<|>))

data DLConf = DLConf {
    image    :: ImageConf
  , training :: TrainConf
  , layers   :: [LayerConf]
  } deriving Show

data ImageConf = ImageConf {
    x  :: Int
  , y  :: Int
  , ch :: Int
  } deriving Show

data TrainConf = TrainConf {
    klass :: Int
  , lerningrate :: Double
  , batchsize   :: Int
  , opstep      :: Int
  , testsize    :: Int
  } deriving Show

data LayerConf = Conv {
    name :: String
  , imgx :: Int
  , imgy :: Int
  , ch_in :: Int
  , kern  :: Int
  , pool  :: Int
  , ch_out :: Int
  } |            Act {



datadir :: Parsec String
datadir = strPara "datadir"

name :: Parsec String
name = strPara "name"

strPara :: String -> Parsec String
strPara id = do
  whiteSpace
  symbol id
  colon
  st <- many1 $ anyChar
  return $ st


-- integer?
intNum :: Parsec Int
intNum = do
  xs <- many $ digitToInt <$> digit
  return $ foldl f 0 xs
  where
    f x y = x * 10 + y

-- float?
doubleNum :: Parsec Double
doubleNum = do
  xs <- many $ digitToDouble <$> digit
  dot <- optionMaybe (char '.')
  ys <- many $ digitToDouble <$> digit
  return $ foldl f 0 xs + foldl g 0 ys
  where
    f x y = x * 10 + y
    g x y = x + y * 0.1
