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
-- Node is of form:             Row Col Team result_g Winner NextNodes
data MoveTree = MTLeaf | MTNode Int Int Char Grid     Char   [MoveTree]

-- function that generates the tree of all possible moves
-- I would comment this but I wrote it at 3am and have no clue what I was thinking or how it works
generateMoveTree :: Grid -> Char -> MoveTree
generateMoveTree g team = Node 0 0 'E' g 'E' (generateMTNodeList (nextMove g team))

generateMTNodeList :: [(Int, Int, Char, Grid)] -> [MoveTree]
generateMTNodeList [] = [MTLeaf]
generateMTNodeList nextMoves = map (generateMTNode) nextMoves

generateMTNode :: (Int, Int, Char, Grid) -> MoveTree
generateMTNode (row, col, team, g) =
  Node row col team g (getWinner g) (generateMTNodeList (nextMove g (incTeam team)))


---------------------------------------------------------------------------------------
-- Functions that power the AI

-- data type for tree holding win percent info derived from move tree
-- Node is of the form:              Row Col Team Perc  NextNodes
data WinPercTree = WPTLeaf | WPTNode Int Int Char Float [WinPercTree]

-- generates WinPercTree from MoveTree
generateWinPercTree :: Char -> MoveTree -> WinPercTree
generateWinPercTree _ MTLeaf  = WPTLeaf
generateWinPercTree cpuTeam (MTNode row col team g winner nextMoves) =
  if cpuTeam == winner then
    WPTNode row col team 1 [WPTLeaf]
  else if winner == 'T' then
    WPTNode row col team 0.5 [WPTLeaf]
  else if winner == 'E' then
    let nextList = (map (generateWinPercTree cpuTeam) nextMoves) in
    WPTNode row col team (calcAverageWinPerc (nextList)) nextList
  else
    WPTNode row col team 0 [WPTLeaf]

-- function to pull win percent from WinPercTree Node
getWinPerc :: WinPercTree -> Maybe Float
getWinPerc WPTLeaf = Nothing
getWinPerc (WPTNode _ _ _ perc _) = Just perc

-- functions to average maybe floats
sumMaybeFloat :: [Maybe Float] -> Float
sumMaybeFloat l = foldl (foldExtractFloat) 0 l

foldExtractFloat :: Float -> Maybe Float -> Float
foldExtractFloat a Nothing = a
foldExtractFloat a (Just x) = a + x

calcAverageWinPerc :: [WinPercTree] -> Float
calcAverageWinPerc wptList =
  let wpList = (map (getWinPerc) wptList) in
    (sumMaybeFloat wpList) / (fromIntegral (length wpList))


    
