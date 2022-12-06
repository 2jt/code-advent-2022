module Day1 (day1part1, day1part2) where

import Prelude

import Data.Foldable (foldr, maximum, sum)
import Data.Int (fromString)
import Data.List (List(..), head, sort, tail, takeEnd, (:))
import Data.Maybe (fromMaybe)
import Data.String (Pattern(..), split)
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

day1part1 :: Effect Unit
day1part1 =
  readTextFile UTF8 "input-day-1" <#> split (Pattern "\n")
    >>= foldr f Nil >>> maximum >>> show >>> log
  where
  f "" list = 0 : list
  f a list = fromMaybe 0 (head list + fromString a) : fromMaybe Nil (tail list)

day1part2 :: Effect Unit
day1part2 =
  readTextFile UTF8 "input-day-1" <#> split (Pattern "\n")
    >>= foldr f Nil >>> sort >>> takeEnd 3 >>> sum >>> show >>> log
  where
  f "" list = 0 : list
  f a list = fromMaybe 0 (head list + fromString a) : fromMaybe Nil (tail list)
