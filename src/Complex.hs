module Complex where

import ApplicativeParser
import Control.Applicative
import Data.Char (digitToInt, isDigit)
import Data.Complex
import Data.Maybe

var :: Char
var = 'z'

cts :: [(String, ET)]
cts =
  [ ("t", 2 * pi),
    ("pi", pi),
    ("e", exp 1),
    ("i", 0 :+ 1),
    ("phi", (1 + sqrt 5) / 2)
  ]

opts :: (Floating a) => [(Char, a -> a -> a)]
opts =
  [ ('+', (+)),
    ('-', (-)),
    ('*', (*)),
    ('/', (/)),
    ('^', (**))
  ]

fcts :: (Floating a) => [(String, a -> a)]
fcts =
  [ ("-", negate),
    ("abs", abs),
    ("exp", exp),
    ("log", log),
    ("sqrt", sqrt),
    ("sin", sin),
    ("cos", cos),
    ("tan", tan),
    ("asin", asin),
    ("acos", acos),
    ("atan", atan),
    ("sinh", sinh),
    ("cosh", cosh),
    ("tanh", tanh),
    ("asinh", asinh),
    ("acosh", acosh),
    ("atanh", atanh)
  ]

type ET = Complex Double

data Expr
  = Val ET
  | Var
  | Op (ET -> ET -> ET) Expr Expr
  | Fun (ET -> ET) Expr

makeOp :: Expr -> Char -> Expr -> Expr
makeOp a o = Op (fromJust (lookup o opts)) a

makeFun :: String -> Expr -> Expr
makeFun f = Fun (fromJust (lookup f fcts))

showComplex :: Complex Double -> String
showComplex (a :+ b) = sa ++ signb ++ sb
  where
    sa = if a == 0 then "" else show a
    signb = case compare b 0 of
      LT -> "-"
      EQ -> ""
      GT -> if a == 0 then "" else "+"
    sb = case abs b of
      0 -> ""
      1 -> "i"
      a -> show a ++ "i"

eval :: Expr -> ET -> ET
eval e p = case e of
  Val i -> i
  Var -> p
  Op op a b -> eval a p `op` eval b p
  Fun f e -> f (eval e p)

parseExpr :: String -> Maybe Expr
parseExpr = runParserUnique exprP . filter (/= ' ')

exprP :: Parser Expr
exprP =
  termP
    <|> makeOp <$> termP <*> anyCharP "+-" <*> exprP

termP :: Parser Expr
termP =
  factorP
    <|> makeOp <$> factorP <*> anyCharP "*/" <*> termP

factorP :: Parser Expr
factorP =
  baseP
    <|> powP
    <|> funP
    <|> absP
    <|> makeOp <$> baseP <*> pure '*' <*> (base'P <|> funP <|> powP)
  where
    funP = makeFun <$> anyStringP (map fst fcts) <*> factorP
    absP = Fun abs <$> (charP '|' *> factorP <* charP '|')
    powP = makeOp <$> baseP <*> charP '^' <*> factorP

baseP :: Parser Expr
baseP = numP <|> base'P

base'P :: Parser Expr
base'P = constP <|> varP <|> charP '(' *> exprP <* charP ')'

varP :: Parser Expr
varP = Var <$ charP var

constP :: Parser Expr
constP = Val . fromJust . flip lookup cts <$> anyStringP (map fst cts)

numP :: Parser Expr
numP = Val . (:+ 0) <$> doubleP

doubleP :: Parser Double
doubleP =
  ( buildDouble <$> emptyP <*> coefFracP
      <|> buildDouble <$> coefIntP <*> emptyP
      <|> buildDouble <$> coefIntP <*> coefFracP
  )
    <*> (emptyP <|> exponentP)
  where
    emptyP = pure []
    coefIntP = digits
    coefFracP = charP '.' *> digits
    exponentP = charP 'E' *> digits
    digits = map digitToInt <$> some (predP isDigit)

    buildInt = sum . zipWith (*) tens . reverse
    buildFrac digits = sum (zipWith div' digits (tail tens))
    buildDouble i f e = (fromIntegral (buildInt i) + buildFrac f) * (10 ^ buildInt e)
    tens = iterate (* 10) 1 :: [Int]
    div' x y = fromIntegral x / fromIntegral y