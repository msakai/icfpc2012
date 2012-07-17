module Main where

import Control.Monad
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO

import Sim
import Interactive

options :: [OptDescr (Options -> Options)]
options =
  [ Option [] ["vi"]   (NoArg (\opt -> opt{ optVi   = True }))  "use vi key-binding"
  , Option [] ["dumb"] (NoArg (\opt -> opt{ optAnsi = False })) "for dumb terminal"
  ]

defaultOptions :: Options
defaultOptions =
  Options
  { optVi   = False
  , optAnsi = True
  }

header :: String
header = "Usage: sim [--vi] file.map"

main :: IO ()
main = do
  args <- getArgs

  case getOpt Permute options args of
    (o,args2,[]) -> do
      let opt = foldl (flip id) defaultOptions o
      case args2 of
        [fname] -> do
          s <- liftM initialStateFromString $ readFile fname
          putStrLn "Commands: :quit, :undo, :reset, :dump"
          putStrLn ""
          interactiveSim opt s
        _ -> do
          hPutStrLn stderr (usageInfo header options)
          exitFailure
    _ -> do
      hPutStrLn stderr (usageInfo header options)
      exitFailure
