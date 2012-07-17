module Main where

import Data.Char
import System.Environment

import Move
import Sim
import Solver 

main :: IO ()
main = do
  prog <- getProgName
  str <- getContents
  let s = initialStateFromString str
  (cmds, score) <- Solver.run s (takeWhile isAlphaNum prog)
  putStrLn $ showCommands cmds
