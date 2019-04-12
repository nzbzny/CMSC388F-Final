{-
   Final Project - Tic-Tac-Toe AI

   Backend
   This file contains the methods for handling the backend of
   the game, including validation and decision making. Import
   it with "import Backend"

   Honor Pledge:
   I pledge on my honor that I have not given or received any
   unauthorized assistance on this assignment.

   Franklin Shedleski
   Noah Zbozny
   Amy Zhao
-}

module Backend where
import DataStructures
import Data.Char

getDigit :: Char -> Int
getDigit c = (ord c) - 48

-- used to validate user input
validInput :: String -> Bool
validInput s =
   if length s == 2 then isDigit (s!!0) && isDigit (s!!1) else False

validMove :: Grid -> String -> Bool
validMove g loc = 
   if validInput loc then
      if getDigit (loc!!0) < 3 && getDigit (loc!!1) < 3 then
         getValue (getRow g (getDigit (loc!!0))) (getDigit (loc!!1)) == 'E'
      else False
   else False

-- returns a grid with the value added at x_pos y_pos == col# row#  (rows and columns are 1 indexed)
grid_add_value_auxX :: Row Char -> Integer -> Integer -> Integer -> Integer -> Char -> Row Char
grid_add_value_auxX [] x_curr y_curr x_goal y_goal value = []
grid_add_value_auxX (g_h:g_t) x_curr y_curr x_goal y_goal value =
  (if x_curr == x_goal && y_curr == y_goal then value else g_h) : (grid_add_value_auxX g_t (x_curr+1) y_curr x_goal y_goal value)
grid_add_value_auxY :: Grid -> Integer -> Integer -> Integer -> Char -> Grid
grid_add_value_auxY [] y_curr x_goal y_goal value = []
grid_add_value_auxY (g_h:g_t) y_curr x_goal y_goal value =
  (grid_add_value_auxX g_h 1 y_curr x_goal y_goal value) : (grid_add_value_auxY g_t (y_curr+1) x_goal y_goal value)
grid_add_value :: Grid -> Integer -> Integer -> Char -> Grid
grid_add_value g_old x_pos y_pos value =
  grid_add_value_auxY g_old 1 x_pos y_pos value

--------------------------------------------------------------------------------------------
-- Functions for validation

-- returns boolean indicating whether the game is over
isGameOver :: Grid -> Bool
isGameOver g = ((getWinner g) /= 'E')

-- returns winner as O, X, T, or E
getWinner :: Grid -> Char
getWinner g =
  if not (isBoardFull g) then
    checkWinner g
  else
    (\x->if x=='E' then 'T' else x) (checkWinner g)

-- used to evaluate whether there is a tie
isBoardFull :: Grid -> Bool
isBoardFull g = foldl (&&) True (map (isRowFull) g)
isRowFull :: Row Char -> Bool
isRowFull r = foldl (\a->(\x->(x/='E')&&a)) True r

-- returns winner as O, X, T, or E
checkWinner :: Grid -> Char
checkWinner g =
  (getWinnerAux (rows g)) `cellOr` (getWinnerAux (cols g)) `cellOr` (getWinnerAux (dias g))
getWinnerAux :: Grid -> Char
getWinnerAux g =
  foldl (cellOr) 'E' (map (getRowWinner) g)
getRowWinner :: Row Char -> Char
getRowWinner r =  
  foldl (cellAnd) '\0' r

----------------------------------------------------------------------------------------
-- Functions to calculate all boards given next move

-- function to increment a set of coordinates in the grid
incCoords :: (Int,Int) -> (Int,Int)
incCoords (3,3) = (1,1)
incCoords (r,3) = (r+1,1)
incCoords (r,c) = (r,c+1)

-- converts a list to a list list where each list has length n
-- this will be used to convert 9 entry lists to 3x3 matricies
-- this operation can be easily reversed with the built in function concat
chunk :: Int -> [a] -> [[a]]
chunk n [] = []
chunk n xs = (take n xs):(chunk n (drop n xs))

-- recursion that generates a list of tuples
-- each tuple holds a broken up flatten list of the grid
-- tuple has form (prefix, entry, suffix)
-- this is used for the list comprehension in next which adds values where entry=='E'
entries :: [x] -> [(Int, Int, [x], x, [x])]
entries xs = entriesAux (1,1) xs
entriesAux :: (Int, Int) -> [x] -> [(Int, Int, [x], x, [x])]
entriesAux _ [] = []
entriesAux (r,c) (x:xs) =
  (r, c, [], x, xs) :
  [(row, col, x:prefix, y, suffix) |
   (row, col, prefix,   y, suffix) <- entriesAux (incCoords (r,c)) xs]

-- list comprehension that uses the results of picks
-- replaces the entry pulled by picks when it equals E
-- the tuple is then converted back to a grid using the chuck method from above
nextMove :: Grid -> Char -> [(Int, Int, Char, Grid)]
nextMove g team =
  [ (row, col, team, (chunk 3 (prefix ++ [team] ++ suffix))) |
    (row, col, prefix, 'E', suffix) <- entries (concat g) ]


-- data type for board tree node
-- tuple is of form (MoveRow, MoveCol, Team, GridAfterMove, WinPercentage, NextNodes)
data TNode = (Int,Int,Char,Grid,[TNode])
