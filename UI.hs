{-
   Final Project - Tic-Tac-Toe AI

   UI
   This file contains functions to draw Grid Objects in the terminal

   Honor Pledge:
   I pledge on my honor that I have not given or received any
   unauthorized assistance on this assignment.

   Frank Shedleski
   Noah Zbozny
   Amy Zhao
-}

module UI where
import DataStructures

instance Show Value where
  show X = "X"
  show O = "O"
  show E = " "

{-
 Should result in a grid that looks like:
 +-+-+-+
 | | | |
 +-+-+-+
 | | | |
 +-+-+-+
 | | | |
 +-+-+-+
 where each blank is appropriately filled

 Call this by typing: "putStrLn $ grid_to_string my_grid"
-}
grid_to_string :: Grid -> String
grid_to_string g =
  foldr (++) "+-+-+-+" $     -- convert list to string
  map ((++) "+-+-+-+\n") $   -- pre-append +-+-+-+ row to every row
  map (foldr (++) "|\n") $   -- convert rows to single strings
  (map (map ((++) "|"))      -- pre-appened | to all
   (map (map show) g))       -- convert all values to strings using show

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
