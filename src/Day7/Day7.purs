module Day7 (day7part1, day7part2) where

import Prelude

import Data.Array (foldl, reverse)
import Data.Foldable (minimum)
import Data.Int (fromString)
import Data.List (List(..), drop, filter, head, (:))
import Data.Maybe (fromMaybe)
import Data.String (Pattern(..), split)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Class.Console (logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

parseLine :: Int /\ Int /\ List Int -> Array String -> Int /\ Int /\ List Int
parseLine a@(sum /\ ans /\ list) = case _ of
  [ "$", "cd", ".." ] -> a
  [ "$", "cd", _ ] -> 0 /\ (if sum <= 100000 then (sum + ans) else ans) /\ sum : list
  [ "dir", _ ] -> (sum + (fromMaybe 0 $ head list)) /\ ans /\ drop 1 list
  [ "$", "ls" ] -> a
  [ size, _ ] -> (sum + (fromMaybe 0 $ fromString size)) /\ ans /\ list
  _ -> a

parseLine2 :: Int /\ List Int /\ List Int -> Array String -> Int /\ List Int /\ List Int
parseLine2 a@(sum /\ ans /\ list) = case _ of
  [ "$", "cd", ".." ] -> a
  [ "$", "cd", _ ] -> 0 /\ (sum : ans) /\ sum : list
  [ "dir", _ ] -> (sum + (fromMaybe 0 $ head list)) /\ ans /\ drop 1 list
  [ "$", "ls" ] -> a
  [ size, _ ] -> (sum + (fromMaybe 0 $ fromString size)) /\ ans /\ list
  _ -> a

solve :: (Array (Array String) -> Effect Unit) -> Effect Unit
solve f =
  readTextFile UTF8 "inputs/input-day-7"
    <#> split (Pattern "\n")
      >>> map (split (Pattern " "))
      >>> reverse
    >>= f

day7part1 :: Effect Unit
day7part1 = solve $ foldl parseLine (0 /\ 0 /\ Nil) >>> logShow

day7part2 :: Effect Unit
day7part2 = solve $ foldl parseLine2 (0 /\ Nil /\ Nil) >>> calc >>> logShow
  where
  calc (_ /\ list /\ (total : Nil)) =
    fromMaybe 0 $ minimum $ filter (_ >= (30000000 - (70000000 - total))) list
  calc _ = 0
