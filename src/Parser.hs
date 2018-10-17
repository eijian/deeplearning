--
-- Parser
--

module Parser (

) where

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Char   as PC
import qualified Text.ParserCombinators.Parsec.Number as PN

--
-- RESERVED WORD
--

rBatchsize :: String
rBatchsize = "batchsize"

rChannelin :: String
rChannelin = "ch_in"

rChannelout :: String
rChannelout = "ch_out"

rClass :: String
rClass = "class"

rFunc :: String
rFunc = "func"

rImage :: String
rImage = "image"

rKernel :: String
rKernel = "kern"

rLearningrate :: String
rLearningrate = "learningrate"

rOutputstep :: String
rOutputstep = "outputstep"

rPool :: String
rPool = "pool"

rSize :: String
rSize = "size"

rTestsize :: String
rTestsize = "testsize"

rTraining :: String
rTraining = "training"

rType :: String
rType = "type"

--
-- PARSERS
--

pname :: String
pname = "rt parser"

charComment :: Char
charComment = '#'

{- |
>>> removeComment "abc"
"abc"
>>> removeComment "ab#"
"ab"
>>> removeComment "abc #   "
"abc "
>>> removeComment "#abc"
""
-}

removeComment :: String -> String
removeComment []     = []
removeComment (c:cs)
  | c == charComment = []
  | otherwise        = c:(removeComment cs)

config :: Parser
config = do
  _  <- spaces
  tr <- trainpart
  _  <- spaces
  ls <- layerpart
  _  <- spaces
  return (tr, ls)


trainpart :: Parser ((Int, Int), Int, Int, Double, Int, Int, Int)
trainpart = do
  _ <- string "training:"
  _ <- eoline
  _ <- many1 space
  im <- image
  ch <- intparam rChannelin
  cl <- intparam rClass
  lr <- learningrate
  bs <- intparam rBatchsize
  ts <- intparam rTestsize
  os <- intparam rOutputstep
  return (im, ch, cl, lr, bs, ts, os)

layerpart :: Parser [Layer]
layerpart = do
  _ <- string "layers:"
  _ <- eoline
  ls <- many1 (try layer)
  return ls

layer :: Parser Layer
layer = do
  l <- (try convolution)   <|>
       (try activation)    <|>
       (try pooling)       <|>
       (try flatten)       <|>
       (try fullconnected)
  return l


--

image :: Parser (Int, Int)
image = do
  _  <- spaces
  _  <- string rImage
  _  <- separator
  i2 <- integer2
  _  <- eoline
  return i2

intparam :: String -> Parser Int
intparam nm = do
  _ <- spaces
  _ <- string nm
  _ <- separator
  i <- integer
  _ <- eoline
  return i

learningrate :: Parser Double
learningrate = do
  _ <- spaces
  _ <- string rLearningrate
  _ <- separator
  f <- float
  _ <- eoline
  return f


--



{- |

}

func :: Parser ActFunc
func = do
  f <- string "step" <|> string "relu" <|> string "relu2" <|> string "softmax"
  return $ case f of
    "step"    -> step
    "relu"    -> relu
    "relu2"   -> relu'
    "softmax" -> softmax
    _         -> error ("invalid function name:" ++ f)

{- |

}

integer2 :: Parser (Int, Int)
integer2 = do
  _ <- string "["
  _ <- many1 space
  i1 <- integer
  _ <- string ","
  _ <- many1 space
  i2 <- integer
  _ <- many1 space
  _ <- string "]"
  return (i1, i2)

{- |
>>> parse float "rt parser" "1"
Right 1.0
>>> parse float "rt parser" "1.0"
Right 1.0
>>> parse float "rt parser" "-1"
Right (-1.0)
>>> parse float "rt parser" "-1.0"
Right (-1.0)
>>> parse float "rt parser" "2.0e3"
Right 2000.0
>>> parse float "rt parser" "-2e-3"
Right (-2.0e-3)
-}

float :: Parser Double
float = do
  s <- PN.sign
  --f <- PN.floating <|> PN.int
  v <- PN.decimalFloat
  let f = case v of
          Left  i  -> fromIntegral (i::Int)
          Right f' -> f'
  return $ s f

{- |
>>> parse integer "rt parser" "1"
Right 1
>>> parse integer "rt parser" "-1"
Right (-1)
>>> parse integer "rt parser" "10"
Right 10
>>> parse integer "rt parser" "300"
Right 300
>>> parse integer "rt parser" "01"
Right 1
-}

integer :: Parser Int
integer = do
  s <- PN.sign
  i <- PN.int
  return $ s i

{- |
>>> parse eoline pname "   \n"
Right ""
>>> parse eoline pname "\t   \n"
Right ""
>>> parse eoline pname "\n"
Right ""
>>> parse eoline pname "bb"
Left "rt parser" (line 1, column 1):
unexpected "b"
expecting lf new-line
-}

eoline :: Parser String
eoline = do
  _ <- many (oneOf " \t")
  _ <- newline
  return ""
