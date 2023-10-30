module TTT.A3 where

import Data.List (transpose)
import TTT.A1
import TTT.A2

-- Q#01

showInts :: [Int] -> [String]
showInts (x : xs) = show x : showInts xs
showInts [] = []

_HEADER_ :: String
_HEADER_ =  " " ++ formatLine (showInts _RANGE_)

-- Q#02

showSquares :: [Square] -> [String]
showSquares (x : xs) = showSquare x : showSquares xs
showSquares [] = []

-- Q#03

formatRows :: [Row] -> [String]
formatRows (x : xs) = formatLine (showSquares x) : formatRows xs
formatRows [] = []

-- Q#04

isColEmpty :: Row -> Int -> Bool
isColEmpty [] _ = False
isColEmpty r 0 = head r == E
isColEmpty (x : xs) i | i > 0 = isColEmpty xs (i - 1)
isColEmpty _ _ = False

-- Q#05

dropFirstCol :: Board -> Board
dropFirstCol (x : xs) = tail x : dropFirstCol xs
dropFirstCol [] = []

dropLastCol :: Board -> Board
dropLastCol (x : xs) = init x : dropLastCol xs
dropLastCol [] = []

-- Q#06

getDiag1 :: Board -> Line
getDiag1 (x : xs) = head x : getDiag1 (dropFirstCol xs)
getDiag1 [] = []

getDiag2 :: Board -> Line
getDiag2 (x : xs) = last x : getDiag2 (dropLastCol xs)
getDiag2 [] = []

getAllLines :: Board -> [Line]
getAllLines b = concat [
    b,
    transpose b,
    [getDiag1 b, getDiag2 b]]

-- Q#07

putSquare :: Player -> Board -> Move -> Board
putSquare player board move
  | isMoveInBounds move =
    let (before,remaining) = splitAt row board
        (row, column) = move in
      case remaining of
        (this:after) -> concat [
            before,
            [replaceSquareInRow player column this],
            after]
        _ -> board
  | otherwise = board

-- Q#08

prependRowIndices :: [String] -> [String]
prependRowIndices rows = prependRowIndices_ $ indexRowStrings rows

prependRowIndices_ :: [(Char, String)] -> [String]
prependRowIndices_ ((index, row) : xs) = (index : row) : prependRowIndices_ xs
prependRowIndices_ [] =[]

-- Q#09

isWinningLine :: Player -> Line -> Bool
isWinningLine p [] = False
isWinningLine p l = isWinningLine_ p l True
isWinningLine_ p xs acc = foldr (\ x -> (&&) (x == p)) acc xs

-- Q#10

isValidMove :: Board -> Move -> Bool
isValidMove board move
  | isMoveInBounds move = isValidMove_ board move
  | otherwise = False

isValidMove_ :: Board -> Move -> Bool
isValidMove_ [] _ = False
isValidMove_ (x:xs) (0, column) = isColEmpty x column
isValidMove_ (x:xs) (row, column) = isValidMove xs (row-1, column)
