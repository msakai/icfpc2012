{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Solver where

import Control.Exception
import Control.Monad
import Data.Char
import Data.List
import Data.IORef
import System.IO

import Move
import Sim
import qualified RandomWalk
import qualified BFS
import qualified DFSGreedy
import qualified MonteCarloTreeSearch

type Algorithm = (GameState -> [Command] -> IO ()) -> GameState -> IO ()

algorithms :: [(String, Algorithm)]
algorithms =
  [ ("random-walk", RandomWalk.run)
  , ("bfs",         BFS.run)
  , ("dfs-greedy",  DFSGreedy.run)
  , ("montecarlo",  MonteCarloTreeSearch.run)
  ]

run :: GameState -> String -> IO ([Command], Int)
run s0 algorithm = do
  let (name, prog) = selectAlgorithm algorithm
  bestRef <- newIORef ([A], 0)
  let -- Commandのリストは逆順なので注意
      check :: GameState -> [Command] -> IO ()
      check s cmds = do
        (_, bestScore) <- readIORef bestRef
        let score = gScore s
        when (score > bestScore) $ do
          hPutStrLn stderr $ "best score = " ++ show score
          hPutStrLn stderr $ "commands = " ++ showCommands (reverse cmds)
             ++ (if gEnd s == Just Winning then " (Win)" else "")
          writeIORef bestRef (cmds, score)

  hPutStrLn stderr $ "algorithm: " ++ name
  result <- try $ prog check s0

  -- XXX
  case result of
    Right () -> return ()
    Left (_ :: AsyncException) -> return ()

  (cmds, score) <- readIORef bestRef
  return (reverse cmds, score)

selectAlgorithm :: String -> (String, Algorithm)
selectAlgorithm prog =
  case xs of
    []  -> head algorithms
    x:_ -> x
  where
    xs = [x | x@(name, _) <- algorithms, downcase prog `isPrefixOf` name]

downcase :: String -> String
downcase = map toLower
