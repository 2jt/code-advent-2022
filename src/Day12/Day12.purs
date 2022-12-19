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

findS :: Array (Array Char) -> Int -> Pos /\ Char
findS arr col = do
  case arr !! col >>= findIndex (_ == 'S') of
    Just xIndex -> (xIndex /\ col) /\ 'S'
    Nothing -> findS arr (col + 1)

getNeighbour :: Array (Array Char) -> (Pos /\ Char) -> Pos -> Maybe (Pos /\ Char)
getNeighbour arr (sp /\ sv) offset =
  arr !! y >>= (_ !! x) >>= \n -> if (mapToElevation n - mapToElevation sv) <= 1 then Just ((x /\ y) /\ n) else Nothing
  where
  x /\ y = sp + offset

solve :: Array (Array Char) -> Array (Pos /\ Char) -> Set (Pos /\ Char) -> Int -> Int
solve arr queue s distance = do
  let
    q = (queue <#> \i -> fourDirs <#> getNeighbour arr i)
      # concat
      # nub
      # catMaybes
      # filter \item -> Set.member item s # not
  case flip find q \(_ /\ val) -> val == 'E' of
    Just _ -> distance + 1
    Nothing ->
      solve arr q (Set.union s $ Set.fromFoldable q) distance + 1
  where
  fourDirs = [ 1 /\ 0, -1 /\ 0, 0 /\ 1, 0 /\ -1 ]

day12part1 :: Effect Unit
day12part1 = do
  heatMap <- readTextFile UTF8 "inputs/input-day-12" <#> split (Pattern "\n") <#> map toCharArray
  solve heatMap [ findS heatMap 0 ] Set.empty 0 # logShow

day12part2 :: Effect Unit
day12part2 = mempty
