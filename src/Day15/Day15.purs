module Day15 (day15part1, day15part2) where

import Prelude

import Control.Alt ((<|>))
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Int (toNumber)
import Data.List (List(..), (:))
import Data.Ord (abs)
import Data.Set as Set
import Data.Tuple (snd)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Class.Console (logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Parsing (Parser, runParser)
import Parsing.Combinators (manyTill)
import Parsing.Combinators.Array (many)
import Parsing.String (anyChar, char, eof)
import Parsing.String.Basic (intDecimal)

type Pos = Int /\ Int

parse :: Parser String (Array (Pos /\ Pos))
parse = many do
  x <- untilNum
  y <- untilNum
  x' <- untilNum
  y' <- untilNum
  (char '\n' # void) <|> eof
  pure $ (x /\ y) /\ (x' /\ y')
  where
  untilNum = manyTill anyChar (char '=') *> intDecimal

mergeInterval :: List Pos -> List Pos -> List Pos
mergeInterval Nil lst = lst
mergeInterval (h : tail) Nil = mergeInterval tail (h : Nil)
mergeInterval ((l /\ h) : tail) t@((l' /\ h') : tail') =
  if (l <= h') then mergeInterval tail ((l' /\ max h h') : tail')
  else mergeInterval tail ((l /\ h) : t)

sensorWithDiff :: List (Pos /\ Int) -> Pos /\ Pos -> List (Pos /\ Int)
sensorWithDiff s (t /\ t') =
  (t /\ calcDiff t t') : s
  where
  calcDiff (x /\ y) (x' /\ y') =
    abs (x - x') + abs (y - y')

solve :: Array (Pos /\ Pos) -> Int
solve arr =
  mergeInterval (Set.toUnfoldable (foldl g Set.empty sensors)) Nil
    # foldl (\acc (a /\ b) -> abs a + abs b + acc) 0
  where
  sensors = foldl sensorWithDiff Nil arr
  g acc ((sx /\ sy) /\ diff) =
    let
      d = diff - abs (2000000 - sy)
    in
      if (d < 0) then acc
      else Set.insert ((sx - d) /\ (sx + d)) acc

solve2 :: Array (Pos /\ Pos) -> Number
solve2 arr =
  g sensors 0 sensors Set.empty
  where
  sensors = foldl sensorWithDiff Nil arr
  g s row Nil set =
    case mergeInterval (Set.toUnfoldable set) Nil of
      (_ : b : _) -> toNumber (snd b + 1) * 4000000.0 + toNumber row
      _ -> g s (row + 1) s Set.empty
  g s row (((sx /\ sy) /\ diff) : tail) set =
    case diff - abs (row - sy) of
      d
        | d < 0 -> g s row tail set
        | otherwise -> g s row tail (Set.insert ((sx - d) /\ (sx + d)) set)

day15part1 :: Effect Unit
day15part1 = do
  readTextFile UTF8 "inputs/input-day-15" >>= flip runParser parse >>> case _ of
    Left _ -> logShow "Error parsing"
    Right r -> logShow $ solve r

day15part2 :: Effect Unit
day15part2 = do
  readTextFile UTF8 "inputs/input-day-15" >>= flip runParser parse >>> case _ of
    Left _ -> logShow "Error parsing"
    Right r -> logShow $ solve2 r
