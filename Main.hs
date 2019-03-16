{-
   Final Project - Tic-Tac-Toe AI

   Main method
   compile with:  ghc -o final Main.hs UI.hs DataStructures.hs
   run with:      ./final

   Honor Pledge:
   I pledge on my honor that I have not given or received any
   unauthorized assistance on this assignment.

   Frank Shedleski
   Noah Zbozny
   Amy Zhao
-}

module Main where
import Data.Char
import DataStructures
import UI


-- completely untested main method I made to test if drawing the stuff works ~Frank
main :: IO ()
main = do
  let g1 = [[E,E,E],[E,E,E],[E,E,E]]
  putStrLn "Enter row to put symbol (1/2/3)"
  y_string <- getLine
  let y = read y_string :: Integer
  putStrLn "Enter col to put symbol (1/2/3)"
  x_string <- getLine
  let x = read x_string :: Integer
  let g2 = grid_add_value g1 x y X 
  putStrLn $ grid_to_string g2
