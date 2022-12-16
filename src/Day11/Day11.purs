module Day11 (day11part1, day11part2) where

import Prelude

import Control.Alt ((<|>))
import Data.Array (concat, length, modifyAt, replicate, snoc, sort, takeEnd, (!!), (..))
import Data.Either (either)
import Data.Foldable (foldl, product)
import Data.Int (floor, rem, toNumber)
import Data.List (toUnfoldable)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (snd)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class.Console (logShow)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Parsing (Parser, runParser)
import Parsing.Combinators (manyTill, sepBy)
import Parsing.Combinators.Array (many)
import Parsing.String (anyChar, string)
import Parsing.String.Basic (intDecimal, whiteSpace)

type Items = Array Int
type Operation = Int -> Int
type DivBy = Int
type Test = Int -> Int

data Monkey = Monkey Items Operation Test

instance Show Monkey where
  show (Monkey l op test) =
    "Monkey:" <> show l
      <> " op is: "
      <> show (op 1)
      <> " after test monkey chosen: "
      <> (show $ test 15)

monkeyParser ∷ Parser String Monkey
monkeyParser =
  Monkey
    <$> items
    <*> operation
    <*> test
  where
  items = do
    manyTill anyChar (string "items: ") # void
    intDecimal `sepBy` (string ", ") <#> toUnfoldable
  operation = do
    manyTill anyChar (string "old ") # void
    op <- anyChar
    whiteSpace # void
    n <- Just <$> intDecimal <|> (string "old") *> pure Nothing
    pure $ case op, n of
      '*', Just n' -> (*) n'
      '+', Just n' -> (+) n'
      '*', Nothing -> \old -> old * old
      '+', Nothing -> \old -> old + old
      _, _ -> identity
  test = do
    manyTill anyChar (string "by ") # void
    n <- intDecimal
    manyTill anyChar (string "monkey ") # void
    mt <- intDecimal
    manyTill anyChar (string "monkey ") # void
    mf <- intDecimal
    pure \x -> if rem x n == 0 then mt else mf

mk20Rounds :: Array Monkey -> Array Int
mk20Rounds monkeys = foldl fn (monkeys /\ rez) loopingRange # snd
  where
  rez = replicate (length monkeys) 0
  loopingRange = replicate 20 (0 .. (length monkeys - 1)) # concat
  fn r@(monkeys' /\ rez') index = fromMaybe r do
    Monkey items op test <- monkeys' !! index
    let itemsToWho = items <#> \i -> let calc = floor ((toNumber $ op i) / 3.0) in test calc /\ calc
    newRez <- modifyAt index (_ + length items) rez'
    updatedMonkeys <- foldl giveToMonkeys (Just monkeys') itemsToWho
      >>= modifyAt index (\(Monkey _ b c) -> Monkey [] b c)
    pure $ updatedMonkeys /\ newRez
    where
    giveToMonkeys (Just acc) (monkeyIndex /\ val) =
      modifyAt monkeyIndex (\(Monkey a b c) -> Monkey (snoc a val) b c) acc
    giveToMonkeys acc _ = acc

day11part1 :: Effect Unit
day11part1 =
  readTextFile UTF8 "inputs/input-day-11"
    >>= flip runParser (many monkeyParser)
      >>> either
        (const $ log "Parsing error")
        ( mk20Rounds
            >>> sort
            >>> takeEnd 2
            >>> product
            >>> logShow
        )

day11part2 :: Effect Unit
day11part2 = mempty
