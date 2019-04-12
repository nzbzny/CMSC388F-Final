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

-- returns winner as O, X, or E
getWinner :: Grid -> Char
getWinner g =
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
grid_add_value_auxX :: Row Value -> Integer -> Integer -> Integer -> Integer -> Value -> Row Value
grid_add_value_auxX [] x_curr y_curr x_goal y_goal value = []
grid_add_value_auxX (g_h:g_t) x_curr y_curr x_goal y_goal value =
  (if x_curr == x_goal && y_curr == y_goal then value else g_h) : (grid_add_value_auxX g_t (x_curr+1) y_curr x_goal y_goal value)
grid_add_value_auxY :: Grid -> Integer -> Integer -> Integer -> Value -> Grid
grid_add_value_auxY [] y_curr x_goal y_goal value = []
grid_add_value_auxY (g_h:g_t) y_curr x_goal y_goal value =
  (grid_add_value_auxX g_h 1 y_curr x_goal y_goal value) : (grid_add_value_auxY g_t (y_curr+1) x_goal y_goal value)
grid_add_value :: Grid -> Integer -> Integer -> Value -> Grid
grid_add_value g_old x_pos y_pos value =
  grid_add_value_auxY g_old 1 x_pos y_pos value
