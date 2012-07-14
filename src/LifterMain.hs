module Main where

import Move
import Sim

main :: IO ()
main = do
  str <- getContents
  let s = initialStateFromString str
      cmds = [A]
  putStrLn $ showCommands cmds
