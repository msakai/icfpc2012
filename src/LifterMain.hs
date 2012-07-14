module Main where

import Move
import Sim
import Solver 

main :: IO ()
main = do
  str <- getContents
  let s = initialStateFromString str
  (cmds, score) <- Solver.run s 
  putStrLn $ showCommands cmds
