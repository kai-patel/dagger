{-# LANGUAGE MonadComprehensions #-}
module Main where

import           Control.Applicative
import           Control.Monad
import           Data.Array
import           Data.Char
import qualified Data.Graph                    as G
import qualified Data.Map                      as M
import           System.Process
import           Text.Printf

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
  empty = Parser
    $ \input -> Error $ "An error occurred while parsing at input: " ++ input

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
-- <relation> ::= <target> <- <dependency_list> -> <command> <EOL>
-- <command> ::= <word>*
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

data Relation = Relation String [String] String
  deriving Show
type Relations = [Relation]

item :: Parser Char
item = Parser $ \input -> case input of
  []       -> Error $ "Unexpected end of input: " ++ input
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

spaces :: Parser ()
spaces = [ () | _ <- many (sat isSpace) ]
  where isSpace x = (x == ' ') || (x == '\t')

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

parse' p = do
  spaces
  v <- p
  pure v

token p = do
  v <- p
  spaces
  pure v

ident :: Parser String
ident = many $ sat printable where printable x = (isAlphaNum x || x == '.' || x == '-') && not (isSpace x)

relations :: Parser Relations
relations = some relation

relation :: Parser Relation
relation = do
  target <- token ident
  token (string "<-")
  deps <- token dependency_list
  token (string "->")
  com <- token commandP
  many (string "\n" <|> string "\r\n")
  pure $ Relation target deps (unwords com)

commandP :: Parser [String]
commandP = (ident `sepBy` char ' ')

dependency_list :: Parser [String]
dependency_list = do
  token $ char '('
  d <- plain_dep_list
  token $ char ')'
  pure d

plain_dep_list :: Parser [String]
plain_dep_list = ident `sepBy` (char ' ')

actioned_dep_list :: Parser [String]
actioned_dep_list = ident `sepBy1` (char ' ')

add_suffix :: String -> String -> String
add_suffix suffix word = word ++ suffix

apply_action :: Relation -> (String -> String) -> Relation
apply_action (Relation target deps com) action =
  Relation target (map action deps) com

parseFromFile :: FilePath -> IO Relations
parseFromFile file = do
  rels <- parse (parse' relations) <$> (readFile file)
  case rels of
    Result (a, "") -> pure a
    Error  e       -> error e
    Result (_, b)  -> error $ printf "Unexpected extra input: %s" b

relationToEdge :: Relation -> (String, String, [String])
relationToEdge (Relation t ds com) = (t, com, ds)

graphFromRelations
  :: [Relation]
  -> ( G.Graph
     , G.Vertex -> (String, String, [String])
     , String -> Maybe G.Vertex
     )
graphFromRelations rels = G.graphFromEdges (map relationToEdge rels)

-- | Calculates all the nodes that are part of cycles in a graph.
cyclicNodes :: Array G.Vertex [G.Vertex] -> [G.Vertex]
cyclicNodes graph = map fst . filter isCyclicAssoc . assocs $ graph
  where isCyclicAssoc = uncurry $ reachableFromAny graph

-- | In the specified graph, can the specified node be reached, starting out
-- from any of the specified vertices?
reachableFromAny :: Foldable t => G.Graph -> G.Vertex -> t G.Vertex -> Bool
reachableFromAny graph node = elem node . concatMap (G.reachable graph)

hasCycles :: G.Graph -> Bool
hasCycles graph = (length $ cyclicNodes graph) > 0

main :: IO ()
main = do
  putStrLn $ "Parsing file"
  (graph, nodeFromVertex, vertexFromKey) <- graphFromRelations
    <$> parseFromFile "dagger"

  let y = hasCycles graph
  putStrLn $ show y
  print $ map (nodeFromVertex) (G.reverseTopSort graph)
