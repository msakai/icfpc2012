module Main where

import System.Environment
import System.Exit
import System.IO

import Map
import Move
import Sim

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fname] -> do
      s <- readFile fname
      let m = parseMap s
      interactiveSim m
    _ -> do
      hPutStrLn stderr "Usage: sim file.map"
      exitFailure
