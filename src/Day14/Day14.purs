module Day14 (day14part1, day14part2) where

import Prelude

import Data.Array (foldl, length, nub, replicate, zip, (..))
import Data.Foldable (maximum)
import Data.Int (fromString)
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set as Set
import Data.String (Pattern(..), split)
import Data.String as String
import Data.Tuple (snd)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Class.Console (logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

type Pos = Int /\ Int

startPos :: Pos
startPos = 500 /\ 0

down :: Pos
down = 0 /\ 1

diagLeft :: Pos
diagLeft = -1 /\ 1

diagRight :: Pos
diagRight = 1 /\ 1

mkPointsList :: Array Pos -> Array (Array Int) -> Array Pos
mkPointsList acc line = do
  acc <> (nub $ f [] (List.fromFoldable line))
  where
  f arr ([ x, y ] : [ x', y' ] : tail)
    | x == x' = let r = y .. y' in f (arr <> (replicate (length r) x) `zip` r) ([ x', y' ] : tail)
    | y == y' = let r = x .. x' in f (arr <> (r `zip` (replicate (length r) y))) ([ x', y' ] : tail)
    | otherwise = arr
  f arr (_ : Nil) = arr
  f arr _ = arr

simulateDrop :: Pos -> Set Pos -> Int -> Int -> Int
simulateDrop p arr acc max =
  if snd (p + down) > max then acc
  else if not (Set.member (p + down) arr) then simulateDrop (p + down) arr acc max
  else if not (Set.member (p + diagLeft) arr) then simulateDrop (p + diagLeft) arr acc max
  else if not (Set.member (p + diagRight) arr) then simulateDrop (p + diagRight) arr acc max
  else simulateDrop startPos (Set.insert p arr) (acc + 1) max

simulateDrop2 :: Pos -> Set Pos -> Int -> Int -> Int
simulateDrop2 p arr acc floor =
  if snd (p + down) == floor then simulateDrop2 startPos (Set.insert p arr) (acc + 1) floor
  else if not (Set.member (p + down) arr) then simulateDrop2 (p + down) arr acc floor
  else if not (Set.member (p + diagLeft) arr) then simulateDrop2 (p + diagLeft) arr acc floor
  else if not (Set.member (p + diagRight) arr) then simulateDrop2 (p + diagRight) arr acc floor
  else if p == startPos then acc + 1
  else simulateDrop2 startPos (Set.insert p arr) (acc + 1) floor

day14part1 :: Effect Unit
day14part1 = do
  paths <-
    readTextFile UTF8 "inputs/input-day-14"
      <#> split (Pattern "\n")
      <#> map
        ( split (Pattern "->")
            >>> map String.trim
            >>> map (split (Pattern ","))
            >>> map (map (fromMaybe 0 <<< fromString))
        )
  let usedPoints = foldl mkPointsList [] paths
  let max = fromMaybe 0 $ maximum $ map snd usedPoints
  logShow $ simulateDrop startPos (Set.fromFoldable usedPoints) 0 max

day14part2 :: Effect Unit
day14part2 = do
  paths <-
    readTextFile UTF8 "inputs/input-day-14"
      <#> split (Pattern "\n")
      <#> map
        ( split (Pattern "->")
            >>> map String.trim
            >>> map (split (Pattern ","))
            >>> map (map (fromMaybe 0 <<< fromString))
        )
  let usedPoints = foldl mkPointsList [] paths
  let floor = (fromMaybe 0 $ maximum $ map snd usedPoints) + 2
  logShow $ simulateDrop2 startPos (Set.fromFoldable usedPoints) 0 floor
