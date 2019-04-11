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
--data Value = X | O | E -- Value of each box is X or O or E for empty

-- gets a list of the rows in the grid
rows :: Matrix a -> [Row a]
rows = id

-- gets a list of the cols in the grid
cols :: Matrix a -> [Row a]
cols = transpose

getRow :: Matrix a -> Int -> Row a
getRow g n = if n < 3 then g!!n else []

getValue :: Row a -> Int -> a
getValue r n = 
    r!!n
