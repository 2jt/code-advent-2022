module Day12 (day12part1, day12part2) where

import Prelude

import Data.Array (catMaybes, concat, filter, find, findIndex, nub, (!!))
import Data.Char (toCharCode)
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.String (Pattern(..), split)
import Data.String.CodeUnits (toCharArray)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Class.Console (logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

type Pos = Int /\ Int

mapToElevation :: Char -> Int
mapToElevation = case _ of
  'S' -> toCharCode 'a'
  'E' -> toCharCode 'z'
  a -> toCharCode a

findStart :: Char -> Array (Array Char) -> Int -> Pos /\ Char
findStart c arr col = do
  case arr !! col >>= findIndex (_ == c) of
    Just xIndex -> (xIndex /\ col) /\ c
    Nothing -> findStart c arr (col + 1)

solve :: (Int -> Int -> Boolean) -> Char -> Array (Array Char) -> Array (Pos /\ Char) -> Set (Pos /\ Char) -> Int -> Int
solve canJump target arr queue s distance = do
  let
    q = (queue <#> \i -> fourDirs <#> getNeighbours i)
      # concat
      # nub
      # catMaybes
      # filter \item -> Set.member item s # not
  case flip find q \(_ /\ val) -> val == target of
    Just _ -> distance + 1
    Nothing ->
      solve canJump target arr q (Set.union s $ Set.fromFoldable q) (distance + 1)
  where
  fourDirs = [ 1 /\ 0, -1 /\ 0, 0 /\ 1, 0 /\ -1 ]
  getNeighbours (sp /\ sv) offset =
    arr !! y >>= (_ !! x) >>= \n -> if canJump (mapToElevation n) (mapToElevation sv) then Just ((x /\ y) /\ n) else Nothing
    where
    x /\ y = sp + offset

day12part1 :: Effect Unit
day12part1 = do
  heatMap <- readTextFile UTF8 "inputs/input-day-12" <#> split (Pattern "\n") <#> map toCharArray
  solve (\t' s' -> t' - s' <= 1) 'E' heatMap [ findStart 'S' heatMap 0 ] Set.empty 0 # logShow

day12part2 :: Effect Unit
day12part2 = do
  heatMap <- readTextFile UTF8 "inputs/input-day-12" <#> split (Pattern "\n") <#> map toCharArray
  solve (\t' s' -> s' - t' <= 1) 'a' heatMap [ findStart 'E' heatMap 0 ] Set.empty 0 # logShow

