module Main where

import Control.Monad
import System.Environment
import System.Exit
import System.IO

import Map
import Move
import Sim
import Metadata

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fname] -> do
      s <- liftM initialStateFromString $ readFile fname
      putStrLn "Commands: :quit, :undo, :dump"
      putStrLn ""
      interactiveSim s
    _ -> do
      hPutStrLn stderr "Usage: sim file.map"
      exitFailure
