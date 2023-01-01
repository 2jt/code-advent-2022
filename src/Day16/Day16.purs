module Day16 (day16part1, day16part2) where

import Prelude

import Control.Alt ((<|>))
import Data.Array (find, many)
import Data.CodePoint.Unicode (isLetter)
import Data.Either (Either(..))
import Data.Filterable (filter)
import Data.Foldable (length, maximum)
import Data.List (List(..), catMaybes, elemIndex, nub, zip)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Unfoldable (replicate)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Parsing (Parser, runParser)
import Parsing.Combinators (manyTill, sepBy)
import Parsing.String (anyChar, char, eof, string)
import Parsing.String.Basic (intDecimal, takeWhile)

data V =
  V String Int (List (String /\ Int))

instance Show V where
  show (V id rate lst) = id <> ":" <> show rate <> " [" <> show lst <> "]"

parse :: Parser String (Array V)
parse = many do
  _ <- string "Valve "
  name <- word
  rate <- manyTill anyChar (char '=') *> intDecimal
  _ <- manyTill anyChar (string "valves " <|> string "valve ")
  lst <- word `sepBy` (string ", ")
  _ <- newline
  pure $ V name rate (lst `zip` (replicate (length lst) 1))
  where
  newline = (char '\n' # void) <|> eof
  word = takeWhile isLetter

getV :: Array V -> String -> Maybe V
getV arr id =
  flip find arr \(V id' _ _) -> id == id'

reduceGraph :: Array V -> Array V
reduceGraph arr = do
  (arr <#> \(V vid rate lst) -> V vid rate (loop vid 1 lst lst))
    # filter (\(V id r _) -> id == "AA" || r /= 0)
    # map \(V id r lst) -> V id r (filter isValveZero lst)
  where
  loop _ _ Nil l = l
  loop vid step lst l = do
    let
      lstJoined = (lst <#> \(id /\ _) -> getValveLst id) # catMaybes # join <#> incBy step
        # filter \(id /\ _) -> elemIndex id (l <#> fst) == Nothing && id /= vid
    loop vid (step + 1) lstJoined (nub (l <> lstJoined))
  isValveZero (id /\ _) =
    (flip find arr \(V id' _ _) -> id == id') <#> (\(V _ r _) -> r /= 0) # fromMaybe true
  incBy step (id /\ d) = id /\ (d + step)
  getValveLst id =
    (flip find arr \(V id' _ _) -> id == id') <#> \(V _ _ lst) -> lst

solve :: Array V -> Set String -> Int -> V -> Int
solve _ _ t _ | t <= 0 = 0
solve arr set t (V _ _ lst) = do
  (catMaybes $ lst <#> fst >>> getV arr) `zip` (lst <#> snd)
    <#> go
    # maximum
    # fromMaybe 0
  where
  go (v@(V id r _) /\ d)
    | Set.member id set = 0
    | otherwise = solve arr (Set.insert id set) (t - d - 1) v + (t - d - 1) * r

day16part1 :: Effect Unit
day16part1 = do
  readTextFile UTF8 "inputs/input-day-16" >>= flip runParser parse >>> case _ of
    Left _ -> logShow "Error parsing"
    Right arr -> do
      let arr' = reduceGraph arr
      case getV arr' "AA" of
        Nothing -> logShow "..."
        Just v -> logShow $ solve arr' Set.empty 30 v

day16part2 :: Effect Unit
day16part2 = mempty
