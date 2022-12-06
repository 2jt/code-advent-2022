module Day5 (day5part1, day5part2) where

import Prelude

import Data.Array (catMaybes, foldl, snoc, transpose, (!!))
import Data.Array as Arr
import Data.Int (fromString)
import Data.Maybe (fromMaybe)
import Data.String (Pattern(..), drop, joinWith, split, take)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

parseRow :: String -> Array String
parseRow s = f [] s
  where
  f arr "" = arr
  f arr s' = f (snoc arr (drop 1 s' # take 1)) (drop 4 s')

parseCommand :: String -> Array Int
parseCommand s = split (Pattern " ") s # f
  where
  f [ _, howMany, _, from, _, to ] = [ howMany, from, to ] <#> fromString # catMaybes
  f _ = []

day5part1 :: Effect Unit
day5part1 = do
  input <- readTextFile UTF8 "input-day-5" <#> split (Pattern "\n")
  let
    stack = Arr.take 8 input # Arr.reverse # map parseRow # transpose # map (Arr.filter ((/=) " "))
    commands = Arr.drop 10 input # map parseCommand
    ans = foldl f stack commands # map Arr.last # catMaybes # joinWith mempty
  logShow ans
  where
  f stack [ howMany, from, to ] = fromMaybe []
    ( do
        fromCol <- stack !! (from - 1)
        let itemsFromCol = Arr.takeEnd howMany fromCol # Arr.reverse
        let itemsNewFromCol = Arr.dropEnd howMany fromCol
        toCol <- stack !! (to - 1)
        let itemsNewToCol = toCol <> itemsFromCol
        Arr.updateAt (from - 1) itemsNewFromCol stack >>= Arr.updateAt (to - 1) itemsNewToCol
    )
  f _ _ = []

day5part2 :: Effect Unit
day5part2 = pure unit
