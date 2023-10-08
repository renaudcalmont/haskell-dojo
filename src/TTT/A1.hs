{-# LANGUAGE GADTs #-}
module TTT.A1 where

import Data.Char (toUpper)
import System.Directory (XdgDirectory(XdgCache))
import GHC.IO.Exception (IOErrorType(TimeExpired))

-- Q#01

_SIZE_ = 3

-- Q#02

_DISPLAY_LOGO_ = True

-- Q#03
convertRowIndex :: Char -> Int
convertRowIndex i = fromEnum (toUpper i) - 65

-- Q#04

_INVALID_MOVE_ = (-1, -1)

-- Q#05

_SEP_ = ['_', '|', '_']

-- Q#06

data Square = X | O | Empty
  deriving (Show, Eq)

-- Q#07

data GameState where
  Xwon :: GameState
  Owon :: GameState
  Tie :: GameState
  InProgress :: GameState
  deriving (Show, Eq)

-- Q#08

type Player = Square
type Row = [Square]
type Line = [Square]
type Board = [Row]
type Move = (Int, Int)

-- Q#09

getFirstPlayer :: Bool -> Player
getFirstPlayer cf = if cf then X else O

getFirstPlayer_ cf
 | cf = X
 | otherwise = O

-- Q#10

showGameState :: GameState -> String
showGameState gs = case gs of
  Xwon -> "X won the game"
  Owon -> "O won the game"
  Tie -> "The game is a tie"
  InProgress -> "The game is in progress"

-- Q#11

switchPlayer :: Player -> Player
switchPlayer p
 | p == X = O
 | p == O = X
 | otherwise = Empty 

-- Q#12

showSquare :: Square -> String
showSquare s
 | s == X = "X"
 | s == O = "O"
 | otherwise = "_"