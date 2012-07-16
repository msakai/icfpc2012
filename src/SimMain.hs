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
    ["--vi",fname] -> do
      s <- liftM initialStateFromString $ readFile fname
      putStrLn "Commands: :quit, :undo, :reset, :dump"
      putStrLn ""
      interactiveSim True s
    [fname] -> do
      s <- liftM initialStateFromString $ readFile fname
      putStrLn "Commands: :quit, :undo, :reset, :dump"
      putStrLn ""
      interactiveSim False s
    _ -> do
      hPutStrLn stderr "Usage: sim [--vi] file.map"
      exitFailure
