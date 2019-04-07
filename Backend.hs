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


--options = ['X', 'O', 'E']
--allBoards g = rows g 
