module Main where

import Data.Char
import Data.List
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO

import Move
import Sim
import Solver 

data Options
  = Algo String

header :: String -> String
header prog = "Usage: " ++ prog ++ " [--algorithm=str] < file.map"

options :: [OptDescr Options]
options =
  [ Option ['a'] ["algorithm"] (ReqArg Algo "<algorithm>")
      ("algorithm: " ++ intercalate ", " [name | (name,_) <- algorithms])
  ]

main :: IO ()
main = do
  prog <- getProgName
  let algo0 = takeWhile isAlphaNum prog

  args <- getArgs
  case getOpt Permute options args of
    (o,args2,[]) -> do
      let algo = last $ algo0 : [name | Algo name <- o]

      str <- getContents
      let s = initialStateFromString str
      (cmds, score) <- Solver.run s algo
      putStrLn $ showCommands cmds
    _ -> do
      hPutStrLn stderr (usageInfo (header prog) options)
      exitFailure
