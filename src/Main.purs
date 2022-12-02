module Main where

import Prelude

import Data.Foldable (foldr, maximum)
import Data.Int (fromString)
import Data.List (List(..), fold, head, sort, tail, takeEnd, (:))
import Data.Maybe (fromMaybe)
import Data.Monoid.Additive (Additive(..))
import Data.String (Pattern(..), split)
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)


day1part1 :: Effect Unit
day1part1 =
  readTextFile UTF8 "input.txt" <#> split (Pattern "\n")
   >>= foldr f Nil >>> maximum >>> show >>> log
  where
  f "" list =  0 : list
  f a list = fromMaybe 0 (head list + fromString a) : fromMaybe Nil (tail list)


day1part2 :: Effect Unit
day1part2 =
  readTextFile UTF8 "input.txt" <#> split (Pattern "\n")
   >>= foldr f Nil >>> sort >>> takeEnd 3 >>> map Additive >>> fold >>> show >>> log
  where
  f "" list =  0 : list
  f a list = fromMaybe 0 (head list + fromString a) : fromMaybe Nil (tail list)


main :: Effect Unit
main = day1part2
