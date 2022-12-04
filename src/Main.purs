module Main where

import Prelude

import Data.Array (concat, cons, drop, length, range, splitAt, take, zip)
import Data.Foldable (foldr, maximum, sum)
import Data.Int (fromString)
import Data.List (List(..), fold, head, sort, tail, takeEnd, (:))
import Data.Map (fromFoldable, lookup)
import Data.Maybe (fromMaybe)
import Data.Monoid.Additive (Additive(..))
import Data.Set as Set
import Data.String (Pattern(..), split)
import Data.String.CodeUnits (toCharArray)
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)


day1part1 :: Effect Unit
day1part1 =
  readTextFile UTF8 "input-day-1" <#> split (Pattern "\n")
   >>= foldr f Nil >>> maximum >>> show >>> log
  where
  f "" list =  0 : list
  f a list = fromMaybe 0 (head list + fromString a) : fromMaybe Nil (tail list)


day1part2 :: Effect Unit
day1part2 =
  readTextFile UTF8 "input-day-1" <#> split (Pattern "\n")
   >>= foldr f Nil >>> sort >>> takeEnd 3 >>> map Additive >>> fold >>> show >>> log
  where
  f "" list =  0 : list
  f a list = fromMaybe 0 (head list + fromString a) : fromMaybe Nil (tail list)

-- A for Rock, B for Paper, and C for Scissors
-- X for Rock, Y for Paper, and Z for Scissors
-- 1 for Rock, 2 for Paper, and 3 for Scissors
-- 0 if you lost, 3 if the round was a draw, and 6 if you won
day2part1 :: Effect Unit
day2part1 = do
  readTextFile UTF8 "input-day-2" <#> split (Pattern "\n")
   >>= map f >>> foldr (+) 0 >>> show >>> log
   where 
   f = case _ of
     "A X" -> 4
     "A Y" -> 8
     "A Z" -> 3
     "B X" -> 1
     "B Y" -> 5
     "B Z" -> 9
     "C X" -> 7
     "C Y" -> 2
     "C Z" -> 6
     _ -> 0
     
-- second column means X = loose, Y = draw, Z = win
day2part2 :: Effect Unit
day2part2 = do
  readTextFile UTF8 "input-day-2" <#> split (Pattern "\n")
   >>= map f >>> foldr (+) 0 >>> show >>> log
   where 
   f = case _ of
     "A X" -> 3
     "A Y" -> 4
     "A Z" -> 8
     "B X" -> 1
     "B Y" -> 5
     "B Z" -> 9
     "C X" -> 2
     "C Y" -> 6
     "C Z" -> 7
     _ -> 0
          

day3part1 :: Effect Unit
day3part1 = do
  readTextFile UTF8 "input-day-3" <#> split (Pattern "\n")
   >>= map f
    >>> concat
    >>> sum
    >>> show >>> log
  where
  alphabet = [ 'a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z',
               'A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z' ]  
  m = fromFoldable (zip alphabet (range 1 52))
  f =
    toCharArray >>> (\i -> splitAt ((length i)/2) i)
    >>> (\i -> Set.fromFoldable i.before `Set.intersection` Set.fromFoldable i.after)
    >>> Set.toUnfoldable
    >>> map (flip lookup m)


day3part2 :: Effect Unit
day3part2 = do
  readTextFile UTF8 "input-day-3" <#> split (Pattern "\n")
   >>= f [] 
     >>> map (map (toCharArray >>> Set.fromFoldable)
              >>> foldr Set.intersection (Set.fromFoldable alphabet) 
              >>> Set.toUnfoldable
              >>> map (flip lookup m))
     >>> concat
     >>> sum
     >>> show >>> log
  where
  alphabet = [ 'a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z',
               'A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z' ]  
  m = fromFoldable (zip alphabet (range 1 52))
  f a [] = a
  f a b = f (cons (take 3 b) a) (drop 3 b)

main :: Effect Unit
main = day3part2
