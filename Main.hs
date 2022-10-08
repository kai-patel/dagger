{-# LANGUAGE MonadComprehensions #-}
module Main where

import           Control.Applicative
import           Control.Monad
import           Data.Char
import qualified Data.Graph                    as G
import qualified Data.Map                      as M

data ParseResult a = Error ParseError | Result (a, String)
    deriving Show
type ParseError = String

newtype Parser a = Parser { parse :: String -> ParseResult a }

instance Functor ParseResult where
  fmap f (r) = case r of
    Error  err        -> Error err
    Result (a, input) -> Result (f a, input)

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (Parser a) = Parser $ fmap f . a

instance Applicative Parser where
  pure :: a -> Parser a
  pure x = Parser $ \input -> Result (x, input)

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  p <*> q = p >>= (<$> q)

instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= q = Parser $ \input -> case parse p input of
    Result (r, input') -> parse (q r) input'
    Error  e           -> Error e

instance Alternative Parser where
  empty = Parser $ \input -> Error "empty"

  (<|>) :: Parser a -> Parser a -> Parser a
  p <|> q = Parser $ \input ->
    let f (Error _) = parse q input
        f r         = r
    in  f (parse p input)

instance MonadPlus Parser where
  mzero = empty
  mplus = (<|>)

instance MonadFail Parser where
  fail x = Parser $ \input -> Error x

-- Grammar
-- <relations> ::= <relation> | <relation> <relations>
-- <relation> ::= <target> <- <dependency_list> <EOL>
-- <dependency_list> ::= E | <plain_dep_list> | <open> <actioned_dep_list> <close>
-- <plain_dep_list> ::= <plain_dep> | <plain_dep> " " <plain_dep_list>
-- <actioned_dep_list> ::= <actioned_dep> | <actioned_dep> " " <actioned_dep_list>
-- <open> ::= "("
-- <close> ::= ")"
-- <target> ::= <word> | <word> "." <word>
-- <plain_dep> ::= <word> | <word> "." <word>
-- <actioned_dep> ::= <word> | <word> "." <word>
-- <word> ::= ([a-z] | [A-Z] | [1-9])+

data Relation = Relation String [String]
  deriving Show
type Relations = [Relation]

item :: Parser Char
item = Parser $ \input -> case input of
  []       -> Error "Unexpected end of input"
  (x : xs) -> Result (x, xs)

sat :: (Char -> Bool) -> Parser Char
sat p = item >>= \x -> if p x then pure x else mzero

char :: Char -> Parser Char
char x = sat (== x)

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = lower <|> upper

string :: String -> Parser String
string "" = do
  pure ""

string (x : xs) = do
  char x
  string xs
  pure (x : xs)

wordP :: Parser String
wordP = many (letter <|> digit)

sepBy1 :: Parser a -> Parser b -> Parser [a]
p `sepBy1` s = do
  x  <- p
  xs <- manyP [ y | _ <- s, y <- p ]
  pure (x : xs)

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p s = (p `sepBy1` s) <|> mzero

bracketed :: Parser a -> Parser b -> Parser a -> Parser b
bracketed open p close = do
  _  <- open
  xs <- p
  _  <- close
  pure xs

add_suffix :: String -> String -> String
add_suffix suffix word = word ++ suffix

apply_action :: Relation -> (String -> String) -> Relation
apply_action (Relation target deps) action = Relation target (map action deps)

main :: IO ()
main = undefined
