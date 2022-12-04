module Day3 (day3part1, day3part2) where

import Prelude

import Data.Array (concat, cons, drop, length, range, splitAt, take, zip)
import Data.Foldable (foldr, sum)
import Data.Map (Map, fromFoldable, lookup)
import Data.Set as Set
import Data.String (Pattern(..), split)
import Data.String.CodeUnits (toCharArray)
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

alphabet :: Array Char
alphabet = [ 'a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z',
             'A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z' ]  

charToNumberMap :: Map Char Int
charToNumberMap = fromFoldable (zip alphabet (range 1 52))

day3part1 :: Effect Unit
day3part1 = do
  readTextFile UTF8 "input-day-3" <#> split (Pattern "\n")
   >>= map f
    >>> concat
    >>> sum
    >>> show >>> log
  where
  f =
    toCharArray >>> (\i -> splitAt ((length i)/2) i)
    >>> (\i -> Set.fromFoldable i.before `Set.intersection` Set.fromFoldable i.after)
    >>> Set.toUnfoldable
    >>> map (flip lookup charToNumberMap)


day3part2 :: Effect Unit
day3part2 = do
  readTextFile UTF8 "input-day-3" <#> split (Pattern "\n")
   >>= f [] 
     >>> map (map (toCharArray >>> Set.fromFoldable)
              >>> foldr Set.intersection (Set.fromFoldable alphabet) 
              >>> Set.toUnfoldable
              >>> map (flip lookup charToNumberMap))
     >>> concat
     >>> sum
     >>> show >>> log
  where
  f a [] = a
  f a b = f (cons (take 3 b) a) (drop 3 b)
