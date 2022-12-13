module Day8 (day8part1) where

import Prelude

import Data.Array (drop, length, mapWithIndex, take, transpose, (!!))
import Data.Foldable (maximum)
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
      if ((maximum $ take j row) < Just c || (maximum $ drop (j + 1) row) < Just c) then 1
      else
        let
          row' = fromMaybe mempty (mt !! j)
        in
          if ((maximum $ take i row') < Just c || (maximum $ drop (i + 1) row') < Just c) then 1 else 0
  let
    f i row
      | i == 0 || i == (length m - 1) = length row
      | otherwise = sum $ flip mapWithIndex row (g i row)
  logShow $ sum $ flip mapWithIndex m f
