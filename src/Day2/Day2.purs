module Day2 (day2part1, day2part2) where

import Prelude

import Data.Foldable (foldr)
import Data.String (Pattern(..), split)
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

f :: String -> Int
f = case _ of
  "A X" -> 4
  "A Y" -> 8
  "A Z" -> 3
  "B X" -> 1
  "B Y" -> 5
  "B Z" -> 9
  "C X" -> 7
  "C Y" -> 2
  "C Z" -> 6
  _ -> 0

day2part1 :: Effect Unit
day2part1 = do
  readTextFile UTF8 "inputs/input-day-2" <#> split (Pattern "\n")
    >>= map f >>> foldr (+) 0 >>> show >>> log

day2part2 :: Effect Unit
day2part2 = do
  readTextFile UTF8 "inputs/input-day-2" <#> split (Pattern "\n")
    >>= map f >>> foldr (+) 0 >>> show >>> log
