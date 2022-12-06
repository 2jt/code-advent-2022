module Day6 (day6part1, day6part2) where

import Prelude

import Data.Array (drop, nub, take)
import Data.String.CodeUnits (toCharArray)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

data Loop = NotFound | Found

solve :: Int -> Effect Unit
solve n =
  readTextFile UTF8 "inputs/input-day-6"
    <#> toCharArray >>> f 0 NotFound
    >>= logShow
  where
  f index Found _ = index
  f index NotFound arr = 
    if (take n arr # nub) /= take n arr
    then f (index + 1) NotFound (drop 1 arr)
    else f (index + n) Found []

day6part1 :: Effect Unit
day6part1 = solve 4

day6part2 :: Effect Unit
day6part2 = solve 14
