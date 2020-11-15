module ApplicativeParser where

import Control.Applicative (Alternative (..))
import Data.Bifunctor (Bifunctor (second))

-- | An ambiguous parser.
newtype Parser a = P {unP :: String -> [(String, a)]}

-- | Instances of Parser
instance Functor Parser where
  --fmap :: (a -> b) -> Parser a -> Parser b
  fmap f p = P $ map (second f) . unP p

instance Applicative Parser where
  --pure :: a -> Parser a
  pure a = P $ \str -> [(str, a)]

  --(<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (P pf) <*> (P px) = P $ \str -> concat [map (second fs) (px strs) | (strs, fs) <- pf str]

instance Alternative Parser where
  --empty :: Parser a
  empty = P $ const []

  --(<|>) :: Parser a -> Parser a -> Parser a
  (P p1) <|> (P p2) = P $ \str -> p1 str ++ p2 str

  --many :: Parser a -> Parser [a]
  many p = some p <|> pure []

  --some :: Parser a -> Parser [a]
  some p = (:) <$> p <*> many p

-- | Parse a character only when a predicate matches.
predP :: (Char -> Bool) -> Parser Char
predP p = P $ \str -> case str of
  [] -> []
  (c : cs) -> [(cs, c) | p c]

-- | Succeed only when parsing the given character.
charP :: Char -> Parser Char
charP c = predP (== c)

-- | Succeed only when parsing any of the given characters.
anyCharP :: [Char] -> Parser Char
anyCharP cs = predP (`elem` cs)

-- | Parse a whole string.
stringP :: String -> Parser String
stringP str = case str of
  (c : cs) -> (:) <$> charP c <*> stringP cs
  _ -> pure ""

-- | Parse any of the strings.
anyStringP :: [String] -> Parser String
anyStringP = foldr ((<|>) . stringP) empty

-- | Apply a parser and return all ambiguous results, but only those where the input was fully consumed.
runParser :: Parser a -> String -> [a]
runParser (P p) = map snd . filter (null . fst) . p

-- | Apply a parser and only return a result, if there was only one unambiguous result with output fully consumed.
runParserUnique :: Parser a -> String -> Maybe a
runParserUnique p = justOne . runParser p
  where
    justOne [a] = Just a
    justOne _ = Nothing