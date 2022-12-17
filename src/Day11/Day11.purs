module Day11 (day11part1, day11part2) where

import Prelude

import Control.Alt ((<|>))
import Data.Array (concat, length, modifyAt, replicate, snoc, sort, takeEnd, (!!), (..))
import Data.Either (either)
import Data.Foldable (foldl, product)
import Data.Int (toNumber)
import Data.List (toUnfoldable)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (floor)
import Data.Tuple (snd)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Class.Console (logShow)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Parsing (Parser, runParser)
import Parsing.Combinators (manyTill, sepBy)
import Parsing.Combinators.Array (many)
import Parsing.String (anyChar, string)
import Parsing.String.Basic (intDecimal, number, whiteSpace)

foreign import jsMod :: Number -> Number -> Number

type Items = Array Number
type Operation = Number -> Number
type Test = Number /\ Int /\ Int

data Monkey = Monkey Items Operation Test

monkeyParser ∷ Parser String Monkey
monkeyParser =
  Monkey
    <$> items
    <*> operation
    <*> test
  where
  items = do
    manyTill anyChar (string "items: ") # void
    number `sepBy` (string ", ") <#> toUnfoldable
  operation = do
    manyTill anyChar (string "old ") # void
    op <- anyChar
    whiteSpace # void
    n <- Just <$> number <|> (string "old") *> pure Nothing
    pure $ case op, n of
      '*', Just n' -> (*) n'
      '+', Just n' -> (+) n'
      '*', Nothing -> \old -> old * old
      '+', Nothing -> \old -> old + old
      _, _ -> identity
  test = do
    manyTill anyChar (string "by ") # void
    n <- number
    manyTill anyChar (string "monkey ") # void
    mt <- intDecimal
    manyTill anyChar (string "monkey ") # void
    mf <- intDecimal
    pure (n /\ mt /\ mf)

mkRounds :: Int -> Number -> Array Monkey -> Array Number
mkRounds rounds divBy monkeys = foldl fn (monkeys /\ rez) loopingRange # snd
  where
  commonDiv = foldl (\acc (Monkey _ _ (d /\ _ /\ _)) -> d * acc) 1.0 monkeys
  rez = replicate (length monkeys) 0.0
  loopingRange = replicate rounds (0 .. (length monkeys - 1)) # concat
  fn r@(monkeys' /\ rez') index = fromMaybe r do
    Monkey items op (divider /\ toWhenTrue /\ toWhenFalse) <- monkeys' !! index
    let
      itemsToWho =
        items <#> \i -> do
          let ans = floor ((op i `jsMod` commonDiv) / divBy)
          if (ans `jsMod` divider == 0.0) then toWhenTrue /\ ans
          else toWhenFalse /\ ans
    newRez <- modifyAt index (_ + (toNumber $ length items)) rez'
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
        ( mkRounds 20 3.0
            >>> sort
            >>> takeEnd 2
            >>> product
            >>> logShow
        )

day11part2 :: Effect Unit
day11part2 =
  readTextFile UTF8 "inputs/input-day-11"
    >>= flip runParser (many monkeyParser)
      >>> either
        (const $ log "Parsing error")
        ( mkRounds 10000 1.0
            >>> sort
            >>> takeEnd 2
            >>> product
            >>> logShow
        )
