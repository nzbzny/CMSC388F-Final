{-
   Final Project - Tic-Tac-Toe AI

   DataStructures
   This file contains all the data structures that we are used
   in the project. import it with: "import DataStructures"

   Honor Pledge:
   I pledge on my honor that I have not given or received any
   unauthorized assistance on this assignment.

   Frank Shedleski
   Noah Zbozny
   Amy Zhao
-}

module DataStructures where
import Data.List

-- Data structure that holds a tic-tac-toe board with its entries
-- I took some of this code from the sudoku lecture ~Frank
type Grid = Matrix Char
type Matrix a = [Row a]
type Row a = [a]

-- gets a list of the rows in the grid
rows :: Matrix a -> [Row a]
rows = id

-- gets a list of the cols in the grid
cols :: Matrix a -> [Row a]
cols = transpose

-- gets a list of the diagonals in the grid
dias :: Matrix a -> [Row a]
dias m = (((m!!0)!!0):((m!!1)!!1):((m!!2)!!2):[]):
         (((m!!0)!!2):((m!!1)!!1):((m!!2)!!0):[]):[]

getRow :: Matrix a -> Int -> Row a
getRow g n = if n < 3 then g!!n else []

getValue :: Row a -> Int -> a
getValue r n = r!!n

-- ands the inputs such that if a==b then a else 'E' 
-- '\0' causes the other input to be returned
cellAnd :: Char -> Char -> Char
cellAnd a b =
  if a == b then a
  else if a == '\0' then b
  else if b == '\0' then a
  else 'E'

-- ors the inputs such that if a==b then a
--                          if a=='X && b=='O' then 'E'
--                          if a=='E' then b
--                          if b=='E' then a
cellOr :: Char -> Char -> Char
cellOr a b =
  if a == b then a
  else if a == 'E' then b
  else if b == 'E' then a
  else 'E'
