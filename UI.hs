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
  foldr (\x->(\a->(showRow x) ++ a)) "+-+-+-+" g

-- assembles row using foldr of showCell
showRow :: Row Char -> String
showRow r = "+-+-+-+\n" ++ (foldr (\x->(\a->(showCell x) ++ a)) "|\n" r)

-- add cell wall, used in foldr in showRow
showCell :: Char -> String
showCell x = "|" ++ (showValue x)

-- convert char cell value to a string representation
showValue :: Char -> String
showValue 'X' = "X"
showValue 'O' = "O"
showValue  _  = " "
