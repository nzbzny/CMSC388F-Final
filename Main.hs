{-
   Final Project - Tic-Tac-Toe AI

   Main method
   compile with:  ghc -o final Main.hs
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


-- just a filler main method
main :: IO ()
main = do
  putStrLn "Enter your name"
  y_string <- getLine
  putStrLn "Hi " + y_string
