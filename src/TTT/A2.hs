module TTT.A2 where

import Data.List (intercalate)
import TTT.A1

-- Q#01

promptPlayer :: Player -> String
promptPlayer p = concat [
    "Player ",
    show p,
    "'s turn: enter a row and column position (ex. A1)" ]

-- Q#02

_RANGE_ = [0 .. _SIZE_-1]

-- Q#03

isDigit :: Char -> Bool
isDigit c = c `elem` ['0' .. '9']

readDigit :: Char -> Int
readDigit c = if isDigit c then read [c] else -1

-- Q#04

_EMPTY_ROW_ = replicate _SIZE_ E

_EMPTY_BOARD_ = replicate _SIZE_ _EMPTY_ROW_

-- Q#05

isTied :: Board -> Bool
isTied b = E `notElem` concat b

_TIED_BOARD_ :: Board
_TIED_BOARD_ = [
    [X, O, O]
  , [O, X, X]
  , [O, X, O]
  ]

-- Q#06

indexRowStrings :: [String] -> [(Char, String)]
indexRowStrings a = zip ['A'..] a

-- Q#07

formatLine :: [String] -> String
formatLine a = concat [
    _SEP_,
    intercalate _SEP_ a,
    _SEP_]

-- Q#08

isMoveInBounds :: Move -> Bool
isMoveInBounds (row,column) = row `elem` _RANGE_ && column `elem` _RANGE_

-- Q#09
stringToMove :: String -> Move
stringToMove m = case m of
  [row, column] -> (convertRowIndex row, readDigit column)
  _ -> _INVALID_MOVE_

-- Q#10
replaceSquareInRow :: Player -> Int -> Row -> Row
replaceSquareInRow p i r
  | i `elem` _RANGE_ =
    let (h,t) = splitAt i r in
      case t of
        (_:tt) -> concat [h, [p], tt]
        _ -> r
  | otherwise = r

rsX i r = replaceSquareInRow X i r
rsO i r = replaceSquareInRow O i r