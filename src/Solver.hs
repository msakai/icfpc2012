{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Solver where

import Control.Exception
import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Data.IORef
import System.IO

import Move
import Sim
import qualified RandomWalk
import qualified BFS
import qualified DFSGreedy
import qualified MonteCarloTreeSearch

type Algorithm = (GameState -> IO ()) -> GameState -> IO ()

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
  bestRef <- newIORef s0
  let check :: GameState -> IO ()
      check s = do
        let s'    = if isJust (gEnd s) then s else step s A
            score = gScore s'
            cmds  = reverse $ gHistory s'

        best <- readIORef bestRef
        when (score > gScore best) $ do
          hPutStrLn stderr $ "best score = " ++ show score
          hPutStrLn stderr $ "commands = " ++ showCommands cmds
             ++ (if gEnd s' == Just Winning then " (Win)" else "")
          writeIORef bestRef s'

  hPutStrLn stderr $ "algorithm: " ++ name
  result <- try $ prog check s0

  -- XXX
  case result of
    Right () -> return ()
    Left (_ :: AsyncException) -> return ()

  s <- readIORef bestRef
  return (reverse (gHistory s), gScore s)

selectAlgorithm :: String -> (String, Algorithm)
selectAlgorithm prog =
  case xs of
    []  -> head algorithms
    x:_ -> x
  where
    xs = [x | x@(name, _) <- algorithms, downcase prog `isPrefixOf` name]

downcase :: String -> String
downcase = map toLower
