module Day8 (day8part1, day8part2) where

import Prelude

import Data.Array (concat, drop, filter, length, mapWithIndex, reverse, take, takeWhile, transpose, (!!))
import Data.Foldable (maximum, product)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), split)
import Data.String.CodeUnits (toCharArray)
import Data.Traversable (sum)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

day8part1 :: Effect Unit
day8part1 = do
  m <- readTextFile UTF8 "inputs/input-day-8" <#> split (Pattern "\n") <#> map toCharArray
  let mt = transpose m
  let
    g i row j c =
      let
        row' = fromMaybe mempty (mt !! j)
      in
        ((maximum $ take j row) < Just c || (maximum $ drop (j + 1) row) < Just c)
          ||
            ((maximum $ take i row') < Just c || (maximum $ drop (i + 1) row') < Just c)
  let
    f i row
      | i == 0 || i == (length m - 1) = length row
      | otherwise = length $ filter identity $ flip mapWithIndex row (g i row)
  logShow $ sum $ flip mapWithIndex m f

day8part2 :: Effect Unit
day8part2 = do
  m <- readTextFile UTF8 "inputs/input-day-8" <#> split (Pattern "\n") <#> map toCharArray
  let mt = transpose m
  let
    take' c arr = do
      let len = takeWhile (_ < c) arr # length
      if (len == length arr) then len
      else len + 1
  let
    g i row j c
      | (i == length m - 1 || i == 0 || j == 0 || j == length mt - 1) = 0
      | otherwise =
          let
            row' = fromMaybe mempty (mt !! j)
          in
            product
              [ take j row # reverse # take' c
              , drop (j + 1) row # take' c
              , take i row' # reverse # take' c
              , drop (i + 1) row' # take' c
              ]
  let
    f i row = flip mapWithIndex row (g i row)
  logShow $ maximum $ concat $ flip mapWithIndex m f
