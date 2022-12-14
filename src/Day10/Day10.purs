module Day10 (day10part1, day10part2) where

import Prelude

import Data.Array (drop, elem, last, mapWithIndex, snoc, take, (!!))
import Data.Foldable (foldl, traverse_)
import Data.Int (fromString)
import Data.Maybe (fromMaybe)
import Data.String (Pattern(..), joinWith, split)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

int :: String -> Int
int = fromMaybe 0 <<< fromString

mkArr :: Array Int -> Array String -> Array Int
mkArr arr entry = case entry of
  [ "addx", v ] -> arr <> [ last', last' + int v ]
  [ "noop" ] -> snoc arr last'
  _ -> arr
  where
  last' = fromMaybe 0 $ last arr

day10part1 :: Effect Unit
day10part1 =
  readTextFile UTF8 "inputs/input-day-10" <#> split (Pattern "\n") <#> map (split (Pattern " "))
    >>= foldl mkArr []
      >>> (\arr -> foldl (\acc i -> acc + i * fromMaybe 0 (arr !! (i - 1))) 0 [ 20, 60, 100, 140, 180, 220 ])
      >>> logShow

day10part2 :: Effect Unit
day10part2 =
  readTextFile UTF8 "inputs/input-day-10" <#> split (Pattern "\n") <#> map (split (Pattern " "))
    >>= foldl mkArr []
      >>> partitionBy40 []
      >>> map drawLine
      >>> traverse_ log
  where
  drawLine arr = joinWith mempty $ flip mapWithIndex arr \i el ->
    if elem i [ el - 1, el, el + 1 ] then "█"
    else "."
  partitionBy40 acc [] = acc
  partitionBy40 acc arr =
    partitionBy40 (snoc acc $ take 40 arr) $ drop 40 arr
