module Complex where

import           ApplicativeParser
import           Control.Applicative
import           Data.Char           (digitToInt, isDigit)
import           Data.Complex
import           Data.Maybe

data Expr a = Val a
            | Var
            | Op (a -> a -> a) (Expr a) (Expr a)
            | Fun (a -> a) (Expr a)

prettyfy :: (Show a, Ord a, Num a) => Complex a -> String
prettyfy (a :+ b) = sa ++ signb ++ sb
  where
    sa = if a == 0 then "" else show a

    signb = case compare b 0 of
      LT -> "-"
      EQ -> ""
      GT -> if a == 0 then "" else "+"

    sb = case abs b of
      0 -> ""
      1 -> "i"
      x -> show x ++ "i"

eval :: Expr a -> a -> a
eval e p = case e of
  Val z     -> z
  Var       -> p
  Op op a b -> eval a p `op` eval b p
  Fun f e   -> f (eval e p)


cts :: [(String, Complex Double)]
cts =
  [ ("t"  , 2 * pi)
  , ("pi" , pi)
  , ("e"  , exp 1)
  , ("i"  , 0 :+ 1)
  , ("phi", (1 + sqrt 5) / 2)
  ]

fcts :: (Floating a) => [(String, a -> a)]
fcts =
  [ ("abs"  , abs)
  , ("exp"  , exp)
  , ("log"  , log)
  , ("sqrt" , sqrt)
  , ("sin"  , sin)
  , ("cos"  , cos)
  , ("tan"  , tan)
  , ("asin" , asin)
  , ("acos" , acos)
  , ("atan" , atan)
  , ("sinh" , sinh)
  , ("cosh" , cosh)
  , ("tanh" , tanh)
  , ("asinh", asinh)
  , ("acosh", acosh)
  , ("atanh", atanh)
  ]

type ExprCD = Expr (Complex Double)

parseExpr :: String -> Maybe ExprCD
parseExpr = runParserUnique exprP . filter (/= ' ')

exprP :: Parser ExprCD
exprP = termNegP <|> Op (+) <$> termNegP <* charP '+' <*> exprP

termNegP :: Parser ExprCD
termNegP = termP <|> Op (-) <$> termP <* charP '-' <*> termNegP

termP :: Parser ExprCD
termP = factorP
  <|> flip Op <$> factorP <*> (getOp <$> anyCharP "*/") <*> termP
  where
    getOp c = case c of
          '*' -> (*)
          '/' -> (/)

factorP :: Parser ExprCD
factorP = baseP
  <|> powP
  <|> funP
  <|> absP
  <|> negP
  <|> Op (*) <$> baseP <*> (groupP <|> funP <|> powP)
  where
    powP = Op (**) <$> baseP <* charP '^' <*> factorP
    funP =
      Fun . fromJust . flip lookup fcts <$> anyStringP (map fst fcts) <*> factorP
    absP = Fun abs <$> (charP '|' *> factorP <* charP '|')
    negP = Fun negate <$> (charP '-' *> factorP)

baseP :: Parser ExprCD
baseP = numP <|> groupP

groupP :: Parser ExprCD
groupP = constP <|> varP <|> charP '(' *> exprP <* charP ')'

varP :: Parser ExprCD
varP = Var <$ charP 'z'

constP :: Parser ExprCD
constP = Val . fromJust . flip lookup cts <$> anyStringP (map fst cts)

numP :: Parser ExprCD
numP = Val . (:+ 0) <$> doubleP

doubleP :: Parser Double
doubleP =
  (   buildDouble <$> emptyP   <*> coefFracP
  <|> buildDouble <$> coefIntP <*> emptyP
  <|> buildDouble <$> coefIntP <*> coefFracP
  ) <*> (emptyP <|> exponentP)
  where
    emptyP    = pure []
    coefIntP  = digits
    coefFracP = charP '.' *> digits
    exponentP = charP 'E' *> digits
    digits    = map digitToInt <$> some (predP isDigit)
    buildInt  = sum . zipWith (*) tens . reverse
    buildFrac digits = sum (zipWith div' digits (tail tens))
    buildDouble i f e =
      (fromIntegral (buildInt i) + buildFrac f) * (10 ^ buildInt e)
    tens = iterate (* 10) 1
    div' x y = fromIntegral x / fromIntegral y
