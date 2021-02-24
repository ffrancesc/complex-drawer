{-# LANGUAGE LambdaCase #-}
module ApplicativeParser where

import           Control.Applicative (Alternative (..))
import           Data.Maybe          (listToMaybe)
-- | An ambiguous parser.
newtype Parser a = P { unP :: String -> [(String, a)] }

-- | Instances of Parser
instance Functor Parser where
  fmap f p = P $ map (fmap f) . unP p

instance Applicative Parser where
  pure a = P $ \str -> [(str, a)]

  pf <*> pa = P
    $ \str -> concat [map (fmap f) (unP pa str') | (str', f) <- unP pf str]

instance Alternative Parser where
  empty = P $ const []

  (P p1) <|> (P p2) = P $ \str -> p1 str ++ p2 str

  many p = some p <|> pure []

  some p = (:) <$> p <*> many p

-- | Parse a character only when a predicate matches.
predP :: (Char -> Bool) -> Parser Char
predP p = P $ \case
  ""     -> []
  (c:cs) -> [(cs, c) | p c]

-- | Succeed only when parsing the given character.
charP :: Char -> Parser Char
charP c = predP (== c)

-- | Succeed only when parsing any of the given characters.
anyCharP :: [Char] -> Parser Char
anyCharP cs = predP (`elem` cs)

-- | Parse a whole string.
stringP :: String -> Parser String
stringP  = \case
  (c:cs) -> (:) <$> charP c <*> stringP cs
  _      -> pure ""

-- | Parse any of the strings.
anyStringP :: [String] -> Parser String
anyStringP = foldr ((<|>) . stringP) empty

-- | Apply a parser and return all ambiguous results, but only those where the input was fully consumed.
runParser :: Parser a -> String -> [a]
runParser p = map snd . filter (null . fst) . unP p

-- | Apply a parser and only return a result, if there was only one unambiguous result with output fully consumed.
runParserUnique :: Parser a -> String -> Maybe a
runParserUnique = (listToMaybe . ) . runParser
