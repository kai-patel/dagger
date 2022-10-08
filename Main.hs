module Main where

import           Control.Applicative
import           Control.Monad
import qualified Data.Graph                    as G
import qualified Data.Map                      as M

data ParseResult a = Error ParseError | Result (a, String)
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
  p <*> q = Parser $ \input ->
    let Result (r1, input' ) = parse p input
        Result (r2, input'') = parse q input'
    in  Result (r1 r2, input'')

instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= q = Parser $ \input -> case parse p input of
    Result (r, input') -> parse (q r) input'
    Error  e           -> Error e

instance Alternative Parser where
  empty = Parser $ \input -> Error "Could not parse"

  (<|>) :: Parser a -> Parser a -> Parser a
  p <|> q = Parser $ \input ->
    let r1 = parse p input
        r2 = parse q input
    in  case r1 of
          Error e -> case r2 of
            Error  _ -> Error e
            Result r -> Result r
          Result r -> Result r

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

add_suffix :: String -> String -> String
add_suffix suffix word = word ++ suffix

apply_action :: Relation -> (String -> String) -> Relation
apply_action (Relation target deps) action = Relation target (map action deps)

main :: IO ()
main = undefined
