module Day6 (day6part1) where

import Prelude

import Data.Array (drop, nub, take)
import Data.String.CodeUnits (toCharArray)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

data Loop = NotFound | Found

day6part1 :: Effect Unit
day6part1 =
  readTextFile UTF8 "inputs/input-day-6"
    <#> toCharArray >>> f 0 NotFound
    >>= logShow
  where
  f index Found _ = index
  f index NotFound arr = 
    if (take 4 arr # nub) /= take 4 arr
    then f (index + 1) NotFound (drop 1 arr)
    else f (index + 4) Found []
