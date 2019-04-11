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
  let g_empty = [[E,E,E],[E,E,E],[E,E,E]]
  putStrLn "Enter team (X/O)"
  team_string <- getLine
  let (team1,team2) = case (team_string) of
                        ("X") -> (X,O)
                        ("x") -> (X,O)
                        ("O") -> (O,X)
                        ("o") -> (O,X)
                        (_) -> (X,O)
  putStrLn "Enter # of players (1/2)"
  players_string <- getLine
  let game_func = case (players_string) of
                    "1" -> (one_player_game)
                    "2" -> (two_player_game)
                    _ -> (one_player_game)
  while (True) g_empty team1 team2 (game_func)

while :: Bool -> Grid -> Value -> Value -> (Grid -> Value -> Value -> IO Grid) -> IO ()
while conditional g p1_token p2_token game_func =
  if conditional == False then
    putStrLn $ "Winner is: " ++  (show $ get_winner g)
  else
    do
      g_new <- (game_func g p1_token p2_token)
      while (not (is_game_over g_new)) g_new p1_token p2_token (game_func)

two_player_game :: Grid -> Value -> Value -> IO Grid
two_player_game g p1_t p2_t =
  do
    g_p1 <- player_turn g p1_t
    putStrLn $ grid_to_string g_p1
    if is_game_over g_p1
      then return g_p1
      else do
      g_p2 <- player_turn g_p1 p2_t
      putStrLn $ grid_to_string g_p2
      return g_p2           

one_player_game :: Grid -> Value -> Value -> IO Grid
one_player_game g p1_t com_t =
  do
    g_p1 <- player_turn g p1_t
    putStrLn $ grid_to_string g_p1
    if is_game_over g_p1
      then return g_p1
      else do
      let g_com = computer_turn g_p1 com_t
      putStrLn $ grid_to_string g_com
      return g_com   

-- gets player input and returns a new grid with their token added
player_turn :: Grid -> Value -> IO Grid
player_turn g token =
  do
    putStrLn $ (show token) ++ "\'s turn!"
    putStrLn "Enter row to put symbol (1/2/3)"
    y_string <- getLine
    let y = read y_string :: Integer
    putStrLn "Enter col to put symbol (1/2/3)"
    x_string <- getLine
    let x = read x_string :: Integer
    let g_new = grid_add_value g x y token
    return g_new
  
-- This is a placeholder. Implement it once we have working AI code
computer_turn :: Grid -> Value -> Grid
computer_turn g token =
  grid_add_value g 1 1 token

