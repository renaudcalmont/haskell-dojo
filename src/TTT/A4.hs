module TTT.A4 where

import Data.List (transpose, intercalate)
import TTT.A1
import TTT.A2
import TTT.A3 (getAllLines, putSquare)

-- Q#01

_HEADER_ = " " ++ formatLine (map show _RANGE_)

-- Q#02

showSquares squares = map showSquare squares

-- Q#03

dropFirstCol board = map tail board

-- Q#04

dropLastCol board = map init board

--Q#05

formatRows rows = map (\x -> formatLine (showSquares x)) rows

-- Q#06

isWinningLine_ p l  = if null l then False else null (filter (/= p) l)

-- Q#07

isWinningLine p [] = False
isWinningLine p xs = foldr (\ x -> (&&) (x == p)) True xs

-- Q#08

hasWon :: Player -> Board -> Bool
hasWon player board = foldr (\x -> (||) (isWinningLine player x)) False (getAllLines board)

_X_WIN_ = [ [X, O, O]
          , [O, X, O]
          , [O, O, X]
          ]

_O_WIN_ = [ [O, X, O]
          , [X, X, O]
          , [X, O, O]
          ]

-- Q#09

getGameState :: Board -> GameState
getGameState board
  | hasWon X board = Xwon
  | hasWon O board = Owon
  | foldr (\x -> (||) (E `elem` x)) False board = InProgress
  | otherwise = Tie

playMove :: Player -> Board -> Move -> (GameState, Board)
playMove = undefined

-- Q#10

prependRowIndices :: [String] -> [String]
prependRowIndices rows = zipWith (:) ['A'..] rows

-- Q#11

formatBoard :: Board -> String
formatBoard board = intercalate "\n" (_HEADER_ : prependRowIndices (formatRows board))