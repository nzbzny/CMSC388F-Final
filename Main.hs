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
import Control.Monad
import Backend
import DataStructures
import UI


-- completely untested main method I made to test if drawing the stuff works ~Frank
main :: IO ()
main = do
  let g_empty = [['E','E','E'],['E','E','E'],['E','E','E']]
  putStrLn "Enter # of players (1/2)"
  players_string <- getLine
  let game_func = case (players_string) of
                    "1" -> (one_player_game_setup)
                    "2" -> (two_player_game)
                    _   -> (one_player_game_setup)
  putStrLn "Enter team (X/O)"
  team_string <- getLine
  let (team1,team2) = case (team_string) of
                        ("X") -> ('X','O')
                        ("x") -> ('X','O')
                        ("O") -> ('O','X')
                        ("o") -> ('O','X')
                        (_) -> ('X','O')
  while (True) (game_func) g_empty team1 team2 

while :: Bool -> (Grid -> Char -> Char -> IO Grid) -> Grid -> Char -> Char -> IO ()
while conditional game_func g p1_token p2_token =
  if conditional == False then
    putStrLn $ "Winner is: " ++  (show $ getWinner g)
  else
    do
      g_new <- (game_func g p1_token p2_token)
      putStrLn $ grid_to_string g_new
      while (not (isGameOver g_new)) (game_func) g_new p1_token p2_token 

two_player_game :: Grid -> Char -> Char -> IO Grid
two_player_game g p1_t p2_t =
  do
    (_, _, _, g_p1) <- player_turn g p1_t
    putStrLn $ grid_to_string g_p1
    if isGameOver g_p1
      then return g_p1
      else do
      (_, _, _, g_p2) <- player_turn g_p1 p2_t
      --putStrLn $ grid_to_string g_p2
      return g_p2           

one_player_game_setup :: Grid -> Char -> Char -> IO Grid
one_player_game_setup g p1_t com_t =
  do
    putStrLn $ "Enter team who goes first (X/O)"
    first_string <- getLine
    let first_team = case (first_string) of
                       ("X") -> ('X')
                       ("x") -> ('X')
                       ("O") -> ('O')
                       ("o") -> ('O')
                       (_) -> ('X')
    if first_team == p1_t
      then one_player_game_player_first g p1_t com_t (generateWinPercTree com_t (generateMoveTree g p1_t))
      else one_player_game_com_first g p1_t com_t (generateWinPercTree com_t (generateMoveTree g com_t))
      
one_player_game_player_first :: Grid -> Char -> Char -> WinPercTree -> IO Grid
one_player_game_player_first g p1_t com_t wpt =
  do
    (p1_r, p1_c, _, g_p1) <- player_turn g p1_t
    putStrLn $ grid_to_string g_p1
    if isGameOver g_p1
      then return g_p1
      else do
        putStrLn "Computer\'s turn!"
        let (g_com, wpt_new) = computer_turn g_p1 com_t (reduceWPT wpt p1_r p1_c p1_t)
        putStrLn $ grid_to_string g_com
        if isGameOver g_com
        then return g_com
        else one_player_game_player_first g_com p1_t com_t wpt_new

one_player_game_com_first :: Grid -> Char -> Char -> WinPercTree -> IO Grid
one_player_game_com_first g p1_t com_t wpt =
  do
    putStrLn "Computer\'s turn!"
    let (g_com, wpt_new) = computer_turn g com_t wpt
    putStrLn $ grid_to_string g_com
    if isGameOver g_com
      then return g_com
      else do
        (p1_r, p1_c, _, g_p1) <- player_turn g_com p1_t
        putStrLn $ grid_to_string g_p1
        if isGameOver g_p1
          then return g_p1
          else one_player_game_com_first g_p1 p1_t com_t (reduceWPT wpt_new p1_r p1_c p1_t)

-- gets player input and returns a new grid with their token added
player_turn :: Grid -> Char -> IO (Int, Int, Char, Grid)
player_turn g token =
  do
    putStrLn $ (show token) ++ "\'s turn!"
    putStrLn "Enter row to put symbol (1/2/3)"
    y_string <- getLine
    let y = read y_string :: Int
    putStrLn "Enter col to put symbol (1/2/3)"
    x_string <- getLine
    let x = read x_string :: Int
    if (validMove g (y_string++x_string)) == True 
      then do
        let g_new = grid_add_value g x y token
        return (x,y, token, g_new)
      else player_turn g token 
    --return g_new
  
-- This is a placeholder. Implement it once we have working AI code
computer_turn :: Grid -> Char -> WinPercTree -> (Grid, WinPercTree)
computer_turn g token wpt =
  let (row, col, team, wpt_new) = popNextMove wpt in
    (grid_add_value g row col team, wpt_new)

