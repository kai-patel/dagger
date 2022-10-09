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
  empty = Parser $ \input -> Error "An error occurred while parsing"

  (<|>) :: Parser a -> Parser a -> Parser a
  p <|> q = Parser $ \input ->
    let f (Error _) = parse q input
        f r         = r
    in  f (parse p input)

instance MonadPlus Parser where
  mzero = empty
  mplus = (<|>)

instance MonadFail Parser where
  fail _ = mzero

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
-- <EOL> ::= '\n' | '\r\n'

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
  xs <- many [ y | _ <- s, y <- p ]
  pure (x : xs)

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p s = (p `sepBy1` s) <|> mzero

bracketed :: Parser a -> Parser b -> Parser a -> Parser b
bracketed open p close = do
  open
  xs <- p
  close
  pure xs

relations :: Parser Relations
relations = some relation

relation :: Parser Relation
relation = do
  t <- target
  bracketed (many (string " ")) (string "<-") (many (string " "))
  deps <- dependency_list
  many (string "\n" <|> string "\r\n")
  pure $ Relation t deps

dependency_list :: Parser [String]
dependency_list =
  empty <|> plain_dep_list <|> bracketed (char '(') actioned_dep_list (char ')')

plain_dep_list :: Parser [String]
plain_dep_list = plain_dep `sepBy1` (char ' ')

actioned_dep_list :: Parser [String]
actioned_dep_list = actioned_dep `sepBy1` (char ' ')

target :: Parser String
target =
  do
    name      <- wordP
    dot       <- char '.'
    extension <- wordP
    pure $ name ++ [dot] ++ extension
  <|> wordP

plain_dep :: Parser String
plain_dep = target

actioned_dep :: Parser String
actioned_dep = target

add_suffix :: String -> String -> String
add_suffix suffix word = word ++ suffix

apply_action :: Relation -> (String -> String) -> Relation
apply_action (Relation target deps) action = Relation target (map action deps)

parseFromFile :: FilePath -> IO Relations
parseFromFile file = do
  rels <- parse relations <$> (readFile file)
  case rels of
    Result (a, "") -> pure a
    Error  e       -> error e
    _              -> error "Unexpected extra input"

relationToEdge :: Relation -> (String, String, [String])
relationToEdge (Relation t ds) = (t, t, ds)

graphFromRelations
  :: [Relation]
  -> ( G.Graph
     , G.Vertex -> (String, String, [String])
     , String -> Maybe G.Vertex
     )
graphFromRelations rels = G.graphFromEdges (map relationToEdge rels)

hasCycles :: G.Graph -> Bool
hasCycles graph = any (== True) (map (\v -> G.path graph v v) (G.vertices graph))

main :: IO ()
main = do
  (graph, nodeFromVertex, vertexFromKey) <- graphFromRelations
    <$> parseFromFile "dagger"
  print $ map (nodeFromVertex) (G.topSort graph)
