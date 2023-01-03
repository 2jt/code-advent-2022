module Day16 (day16) where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Error.Class (liftMaybe)
import Data.Array (cons, find, many, partition, take)
import Data.Array as Array
import Data.CodePoint.Unicode (isLetter)
import Data.Either (Either(..))
import Data.Filterable (filter)
import Data.Foldable (elem, length, maximum)
import Data.List (List(..), catMaybes, fromFoldable, nub, zip, (:))
import Data.Maybe (Maybe, fromMaybe)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (fst)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Unfoldable (replicate)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Effect.Exception (error)
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

getV :: Array V -> String -> Maybe V
getV arr id =
  flip find arr \(V id' _ _) -> id == id'

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

reduceGraph :: Array V -> Array V
reduceGraph arr = do
  (arr <#> \(V vid rate lst) -> V vid rate (loop vid 1 lst lst))
    # filter (\(V id r _) -> id == "AA" || r /= 0)
    # map \(V id r lst) -> V id r (filter isValveZero lst)
  where
  loop _ _ Nil l = l
  loop vid step lst l = do
    let lstJ = (lst <#> \(id /\ _) -> getValveLst id) # catMaybes # join <#> incBy step
    let lstJf = flip filter lstJ \(id /\ _) -> not $ elem id (l <#> fst) && id /= vid
    loop vid (step + 1) lstJf (nub (l <> lstJf))
  isValveZero (id /\ _) = fromMaybe true $ getV arr id <#> \(V _ r _) -> r /= 0
  incBy step (id /\ d) = id /\ (d + step)
  getValveLst id = getV arr id <#> \(V _ _ lst) -> lst

solve :: Array V -> Set String -> Int -> V -> Int
solve _ _ t _ | t <= 1 = 0
solve arr set t (V _ _ lst) = do
  let l = catMaybes $ lst <#> \(id /\ d) -> getV arr id <#> (_ /\ d)
  l <#> go # maximum # fromMaybe 0
  where
  go (v@(V id r _) /\ d)
    | Set.member id set = 0
    | otherwise = solve arr (Set.insert id set) (t - d - 1) v + (t - d - 1) * r

solve2 :: Array V -> V -> Int
solve2 arr v = do
  let labels = (arr <#> \(V id _ _) -> id)
  let p = combinations (fromFoldable labels) # \p' -> take ((length p') / 2) p'
  p <#> f # maximum # fromMaybe 0
  where
  f p1Labels = do
    let { yes: p1, no: p2 } = flip partition arr \(V id _ _) -> elem id p1Labels
    (solve p1 Set.empty 26 v) + (solve p2 Set.empty 26 v)
  combinations Nil = []
  combinations (x : Nil) = [ [ x ] ]
  combinations (x : xs) = Array.nub do
    l <- combinations xs
    [ pure x, (cons x l), l ]

day16 :: Effect Unit
day16 = do
  readTextFile UTF8 "inputs/input-day-16" >>= flip runParser parse >>> case _ of
    Left _ -> logShow "Error parsing"
    Right arr -> do
      let arr' = reduceGraph arr
      v <- liftMaybe (error "No AA") $ getV arr' "AA"
      logShow $ solve arr' Set.empty 30 v
      logShow $ solve2 (flip filter arr' \(V id _ _) -> id /= "AA") v
