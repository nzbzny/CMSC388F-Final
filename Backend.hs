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

--options = ['X', 'O', 'E']
--allBoards g = rows g 


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
