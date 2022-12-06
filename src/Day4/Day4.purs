module Day4 (day4part1, day4part2) where

import Prelude

import Data.Array (catMaybes, concat, range)
import Data.Foldable (sum)
import Data.Int (fromString)
import Data.Set as Set
import Data.String (Pattern(..), split)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

day4part1 :: Effect Unit
day4part1 = do
  readTextFile UTF8 "input-day-4" <#> split (Pattern "\n")
    >>=
      map
        ( split (Pattern ",")
            >>> map (split (Pattern "-"))
            >>> concat
            >>> map fromString
            >>> catMaybes
            >>> f
        )
        >>> sum
        >>> logShow
  where
  f [ a, b, x, y ] = if Set.subset s1 s2 || Set.subset s2 s1 then 1 else 0
    where
    s1 = Set.fromFoldable (range a b)
    s2 = Set.fromFoldable (range x y)
  f _ = 0

day4part2 :: Effect Unit
day4part2 = do
  readTextFile UTF8 "input-day-4" <#> split (Pattern "\n")
    >>=
      map
        ( split (Pattern ",")
            >>> map (split (Pattern "-"))
            >>> concat
            >>> map fromString
            >>> catMaybes
            >>> f
        )
        >>> sum
        >>> logShow
  where
  f [ a, b, x, y ] = if Set.isEmpty (Set.intersection s1 s2) then 0 else 1
    where
    s1 = Set.fromFoldable (range a b)
    s2 = Set.fromFoldable (range x y)
  f _ = 0
