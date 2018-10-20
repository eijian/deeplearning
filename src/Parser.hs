--
-- Parser
--

module Parser (
  removeComment
, config
, Config
, LayerB
, parse
) where

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Char   as PC
import qualified Text.ParserCombinators.Parsec.Number as PN

import CNN.ActLayer
import CNN.ConvLayer
import CNN.FullConnLayer
import CNN.Layer
import CNN.LayerType
import CNN.PoolLayer

--
-- TYPES
--

type Config = ((Int, Int), Int, Int, Double, Int, Int, Int, Int)


--
-- RESERVED WORD
--

rActivation :: String
rActivation = "activation"

rBatchsize :: String
rBatchsize = "batchsize"

rChannelin :: String
rChannelin = "ch_in"

rChannelout :: String
rChannelout = "ch_out"

rClass :: String
rClass = "class"

rConvolution :: String
rConvolution = "convolution"

rFlatten :: String
rFlatten = "flatten"

rFullconnected :: String
rFullconnected = "fullconnected"

rFunc :: String
rFunc = "func"

rImage :: String
rImage = "image"

rInit :: String
rInit = "init"

rKernel :: String
rKernel = "kern"

rLearningrate :: String
rLearningrate = "learningrate"

rPool :: String
rPool = "pool"

rPooling :: String
rPooling = "pooling"

rRepeat :: String
rRepeat = "repeat"

rSavepoint :: String
rSavepoint = "savepoint"

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

config :: Parser (Config, [LayerB])
config = do
  _  <- spaces
  tr <- trainpart
  _  <- spaces
  ls <- layerpart
  _  <- spaces
  return (tr, ls)

trainpart :: Parser Config
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
  rp <- intparam rRepeat
  sp <- intparam rSavepoint
  return (im, ch, cl, lr, bs, ts, rp, sp)

layerpart :: Parser [LayerB]
layerpart = do
  _ <- string "layers:"
  _ <- eoline
  ls <- many1 (try layer)
  return ls

layer :: Parser LayerB
layer = do
  l <- (try convolution)   <|>
       (try activation)    <|>
       (try pooling)       <|>
       (try flatten)       <|>
       (try fullconnected)
  return l

{- |
convolution
>>> parse convolution pname "  - type: convolution\n  image: [ 12, 12 ]\n  ch_in: 1\n  ch_out: 10\n  kern: 3\n  pool: 2\n"
Right ("convolution",[12,12,1,10,3,2])
-}

convolution :: Parser LayerB
convolution = do
  t <- typecheck rConvolution
  (x, y) <- image
  ci <- intparam rChannelin
  co <- intparam rChannelout
  kn <- intparam rKernel
  pl <- intparam rPool
  return (t, [x, y, ci, co, kn, pl])
  --ff <- initFilterC co ci x y kn pl
  --return $ ConvLayer kn ff

{- |
activation
>>> parse activation pname "  - type: activation\n  func: step\n"
Right ("activation",[0])
>>> parse activation pname "  - type: activation\n  func: relu\n"
Right ("activation",[1])
>>> parse activation pname "  - type: activation\n  func: relu'\n"
Right ("activation",[2])
>>> parse activation pname "  - type: activation\n  func: softmax\n"
Right ("activation",[3])
>>> parse activation pname "  - type: activation\n  func: aaa\n"
Left "rt parser" (line 2, column 9):
unexpected "a"
expecting "step", "relu'", "relu" or "softmax"

-}

activation :: Parser LayerB
activation = do
  t <- typecheck rActivation
  _ <- spaces
  _ <- string rFunc
  _ <- separator
  fc <- func'
  --return $ ActLayer fc
  return (t, [fc])

{- |
pooling
>>> parse pooling pname "  - type: pooling\n  size: 3\n"
Right ("pooling",[3])
-}

pooling :: Parser LayerB
pooling = do
  t <- typecheck rPooling
  s <- intparam rSize
  -- return $ MaxPoolLayer s
  return (t, [s])

{- |
flatten
>>> parse flatten pname "  - type: flatten\n    image: [ 10, 10 ]\n"
Right ("flatten",[10,10])

-}

flatten :: Parser LayerB
flatten = do
  t <- typecheck rFlatten
  (x, y) <- image
  -- return $ FlattenLayer x y
  return (t, [x, y])

{- |
fullconnected
>>> parse fullconnected pname "  - type: fullconnected\n    ch_in: 1\n ch_out: 10\n  init: rand\n"
Right ("fullconnected",[1,10,1])

-}

fullconnected :: Parser LayerB
fullconnected = do
  t <- typecheck rFullconnected
  ci <- intparam rChannelin
  co <- intparam rChannelout
  _ <- spaces
  _ <- string rInit
  _ <- separator
  it <- zerorand
{-
  mkfullconnlayer it co ci
  let
    ff <- if it == True  -- "zero" is true
      then zeroFilterF co ci
      else initFilterF co ci
  return $ FullConnLayer ff
-}
  return (t, [ci, co, if it == True then 0 else 1])

mkfullconnlayer :: Bool -> Int -> Int -> IO Layer
mkfullconnlayer it co ci = do
  ff <- if it == True
    then zeroFilterF co ci
    else initFilterF co ci
  return $ FullConnLayer ff

--

typecheck :: String -> Parser String
typecheck tp = do
  _ <- many1 space
  _ <- char '-'
  _ <- many1 space
  _ <- string rType
  _ <- separator
  t <- string tp
  _ <- eoline
  return t

{- |
image
>>> parse image pname "image: [ 2, 3 ]\n"
Right (2,3)
-}

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

{- |
learningrate
>>> parse learningrate pname "learningrate: 0.5\n"
Right 0.5
>>> parse learningrate pname "learningrate: 1\n"
Right 1.0
>>> parse learningrate pname "learningrate : 0.5\n"
Right 0.5
>>> parse learningrate pname "learningrate:0.5\n"
Left "rt parser" (line 1, column 14):
unexpected "0"
>>> parse learningrate pname "learningrate: 0.5"
Left "rt parser" (line 1, column 18):
unexpected end of input
expecting digit, exponent or lf new-line
>>> parse learningrate pname "learningrate: a\n"
Left "rt parser" (line 1, column 15):
unexpected "a"
expecting "-", "+" or digit

-}

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
func'
>>> parse func' pname "step"
Right 0
>>> parse func' pname "relu"
Right 1
>>> parse func' pname "relu'"
Right 2
>>> parse func' pname "softmax"
Right 3
>>> parse func' pname "aaa"
Left "rt parser" (line 1, column 1):
unexpected "a"
expecting "step", "relu'", "relu" or "softmax"

-}

func :: Parser ActFunc
func = do
  f <- (try $ string "step")    <|>
       (try $ string "relu'")   <|>
       (try $ string "relu")    <|>
       (try $ string "softmax")
  return $ case f of
    "step"    -> step
    "relu"    -> relu
    "relu'"   -> relu'
    "softmax" -> softmax
    _         -> error ("invalid function name:" ++ f)

func' :: Parser Int
func' = do
  f <- (try $ string "step")    <|>
       (try $ string "relu'")   <|>
       (try $ string "relu")    <|>
       (try $ string "softmax")
  return $ case f of
    "step"    -> 0
    "relu"    -> 1
    "relu'"   -> 2
    "softmax" -> 3
    _         -> error ("invalid function name:" ++ f)

{- |
integer2
>>> parse integer2 pname "[ 1, 2 ]"
Right (1,2)
>>> parse integer2 pname "[1, 2 ]"
Left "rt parser" (line 1, column 2):
unexpected "1"
expecting space
>>> parse integer2 pname "[ 1,2 ]"
Left "rt parser" (line 1, column 5):
unexpected "2"
expecting space
>>> parse integer2 pname "[ 1, 2]"
Left "rt parser" (line 1, column 7):
unexpected "]"
expecting digit or space
>>> parse integer2 pname "[ a, 2 ]"
Left "rt parser" (line 1, column 3):
unexpected "a"
expecting space, "-", "+" or digit
>>> parse integer2 pname "[ 1, b ]"
Left "rt parser" (line 1, column 6):
unexpected "b"
expecting space, "-", "+" or digit
>>> parse integer2 pname " [ 1, 2 ]"
Left "rt parser" (line 1, column 1):
unexpected " "
expecting "["
>>> parse integer2 pname "[ 1, 2 ] "
Right (1,2)

-}

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
>>> parse zerorand pname "zero"
Right True
>>> parse zerorand pname "rand"
Right False
>>> parse zerorand pname "xxx"
Left "rt parser" (line 1, column 1):
unexpected "x"
expecting "zero" or "rand"
-}

zerorand :: Parser Bool
zerorand = do
  s <- string "zero" <|> string "rand"
  return $ if s == "zero" then True else False

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

{- |
>>> parse separator pname ": "
Right ""
>>> parse separator pname " : "
Right ""
>>> parse separator pname "     : "
Right ""
>>> parse separator pname " :"          -- YAML grammer error
Left "rt parser" (line 1, column 3):
unexpected end of input
>>> parse separator pname ":"           -- YAML grammer error
Left "rt parser" (line 1, column 2):
unexpected end of input
-}

separator :: Parser String
separator = do
  _ <- many space
  _ <- string ":"
  _ <- many1 (oneOf " \t")
  return ""

