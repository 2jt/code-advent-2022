module Day9 (day9part1, day9part2) where

import Prelude

import Data.Foldable (foldl)
import Data.Int (fromString)
import Data.List (List(..), head, length, nub, range, reverse, (:))
import Data.Maybe (fromMaybe)
import Data.Ord (abs)
import Data.String (Pattern(..), split)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class.Console (logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

type Pos = Tuple Int Int

type Acc =
  { headPos :: Pos
  , tailPosHistory :: List Pos
  }

initState :: List Pos
initState = (0 /\ 0) : Nil

int :: String -> Int
int = fromMaybe 0 <<< fromString

mkTailMoves :: Pos -> List Pos -> List Pos
mkTailMoves diff@(x /\ y) list@(tailPos : _)
  | abs x <= 1 && abs y <= 1 = list
  | otherwise =
      mkTailMoves
        (diff + (-1 * (abs x) / x /\ -1 * (abs y) / y))
        ((tailPos + ((abs x) / x /\ (abs y) / y)) : list)
mkTailMoves _ list = list

parseMove :: Array String -> Pos
parseMove = case _ of
  [ "R", steps ] -> int steps /\ 0
  [ "L", steps ] -> (int steps # negate) /\ 0
  [ "U", steps ] -> 0 /\ int steps
  [ "D", steps ] -> 0 /\ int steps # negate
  _ -> 0 /\ 0

accumulate :: Acc -> Array String -> Acc
accumulate { headPos, tailPosHistory: list@(lastPos : _) } command =
  { headPos: newHeadPos
  , tailPosHistory: mkTailMoves (newHeadPos - lastPos) list
  }
  where
  newHeadPos = headPos + parseMove command
accumulate acc _ = acc

day9part1 :: Effect Unit
day9part1 = do
  readTextFile UTF8 "inputs/input-day-9" <#> split (Pattern "\n") <#> map (split (Pattern " "))
    >>=
      foldl accumulate
        { headPos: 0 /\ 0
        , tailPosHistory: initState
        }
        >>> _.tailPosHistory
        >>> nub
        >>> length
        >>> logShow

followPoint :: List Pos -> Pos -> List Pos
followPoint list@(h : _) pos = case pos - h of
  x /\ y
    | abs x <= 1 && abs y <= 1 -> list
    | otherwise -> ((h + ((abs x) / x /\ (abs y) / y)) : list)
followPoint list _ = list

day9part2 :: Effect Unit
day9part2 =
  readTextFile UTF8 "inputs/input-day-9" <#> split (Pattern "\n") <#> map (split (Pattern " "))
    >>=
      foldl accumulate
        { headPos: 0 /\ 0
        , tailPosHistory: initState
        }
        >>> _.tailPosHistory
        >>> (\a -> foldl (\x _ -> foldl followPoint initState (reverse x)) a (range 2 9))
        >>> nub
        >>> length
        >>> logShow

