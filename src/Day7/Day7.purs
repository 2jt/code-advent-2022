module Day7 (day7part1, day7part2) where

import Prelude

import Data.Array (foldl, reverse)
import Data.Int (fromString)
import Data.List (List(..), drop, head, (:))
import Data.Maybe (fromMaybe)
import Data.String (Pattern(..), split)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Class.Console (logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

parseLine :: (Int /\ Int /\ List Int) -> Array String -> (Int /\ Int /\ List Int)
parseLine a@(sum /\ ans /\ list) arr = case arr of
  [ "$", "cd", ".." ] -> a
  [ "$", "cd", _ ] -> (0 /\ (if sum <= 100000 then (sum + ans) else ans) /\ sum : list)
  [ "dir", _ ] -> ((sum + (fromMaybe 0 $ head list)) /\ ans /\ drop 1 list)
  [ "$", "ls" ] -> a
  [ size, _ ] -> ((sum + (fromMaybe 0 $ fromString size)) /\ ans /\ list)
  _ -> a

day7part1 :: Effect Unit
day7part1 = do
  readTextFile UTF8 "inputs/input-day-7"
    <#> split (Pattern "\n")
      >>> map (split (Pattern " "))
      >>> reverse
    >>= foldl parseLine (0 /\ 0 /\ Nil)
      >>> logShow

day7part2 :: Effect Unit
day7part2 = mempty

