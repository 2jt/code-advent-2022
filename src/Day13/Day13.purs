module Day13 (day13part1, day13part2) where

import Prelude

import Control.Alt ((<|>))
import Data.Array (elemIndex, foldl, many, mapWithIndex, sort)
import Data.Either (Either(..))
import Data.Foldable (sum)
import Data.List (List(..), (:))
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Class.Console (logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Parsing (Parser, runParser)
import Parsing.Combinators (sepBy)
import Parsing.String (char, eof)
import Parsing.String.Basic (intDecimal)

data Entry
  = Val Int
  | Lst (List Entry)

parsePair ∷ Parser String (Entry /\ Entry)
parsePair = do
  a <- parseLst
  newline
  b <- parseLst
  newline *> newline <|> eof
  pure $ a /\ b
  where
  newline = char '\n' # void
  parseLst = do
    char '[' # void
    lst <- ((intDecimal <#> Val) <|> parseLst) `sepBy` (char ',') <#> Lst
    char ']' # void
    pure lst

instance Ord Entry where
  compare (Val v) (Val v') = compare v v'
  compare (Lst l) (Lst l') = compare l l'
  compare (Val v) l = compare (Lst (Val v : Nil)) l
  compare l (Val v) = compare l (Lst (Val v : Nil))

instance Eq Entry where
  eq a b = compare a b == EQ

day13part1 :: Effect Unit
day13part1 = do
  input <- readTextFile UTF8 "inputs/input-day-13"
  case runParser input (many parsePair) of
    Left e -> logShow $ "Error!" <> show e
    Right r ->
      logShow $ sum $ flip mapWithIndex r (\i (e /\ e') -> if (e <= e') then i + 1 else 0)

day13part2 :: Effect Unit
day13part2 = do
  input <- readTextFile UTF8 "inputs/input-day-13"
  case runParser input (many parsePair) of
    Left e -> logShow $ "Error!" <> show e
    Right r ->
      let
        a = sort $ foldl (\acc (e /\ e') -> [ e, e' ] <> acc) [ add1, add2 ] r
      in
        logShow $ (elemIndex add1 a + pure 1) * (elemIndex add2 a + pure 1)
  where
  add1 = Lst ((Lst (Val 2 : Nil)) : Nil)
  add2 = Lst ((Lst (Val 6 : Nil)) : Nil)
