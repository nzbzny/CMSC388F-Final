{-
   Final Project - Tic-Tac-Toe AI

   Backend
   Has the code to support the computer player

   Honor Pledge:
   I pledge on my honor that I have not given or received any
   unauthorized assistance on this assignment.

   Frank Shedleski
   Noah Zbozny
   Amy Zhao
-}

module Backend where
import DataStructures

-- This is a placeholder so the UI functions.
is_game_over :: Grid -> Bool
is_game_over g =
  foldl (&&) True (map (foldl (&&) True ) (map (map ((/=) E)) g))

-- This is a placeholder so the UI functions.
get_winner :: Grid -> Value
get_winner g = X
