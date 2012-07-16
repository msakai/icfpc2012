{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Solver where

import Control.Exception
import Control.Monad
import Data.Char
import Data.IORef
import System.IO
import System.Environment

import Move
import Sim
import qualified RandomWalk
import qualified BFS
import qualified DFSGreedy
import qualified MonteCarloTreeSearch

import Debug.Trace

run :: GameState -> IO ([Command], Int)
run s0 = do
  prog <- selectAlgorithm
  bestRef <- newIORef ([A], 0)
  let -- Commandのリストは逆順なので注意
      check :: GameState -> [Command] -> IO ()
      check s cmds = do
        (_, bestScore) <- readIORef bestRef
        let score = gScore s
        when (score > bestScore) $ do
          hPutStrLn stderr $ "best score = " ++ show score
          hPutStrLn stderr $ "commands = " ++ showCommands (reverse cmds)
          writeIORef bestRef (cmds, score)
  result <- try $ prog check s0
--  result <- try $ RandomWalk.run check s0
--  result <- try $ BFS.run check s0
--  result <- try $ DFSGreedy.run check s0

  -- XXX
  case result of
    Right () -> return ()
    Left (_ :: AsyncException) -> return ()

  (cmds, score) <- readIORef bestRef
  return (reverse cmds, score)


selectAlgorithm :: IO ((GameState -> [Command] -> IO ()) -> GameState -> IO ())
selectAlgorithm = do
  { prog <- getProgName
  ; return $ case downcase $ take 6 prog of
      "random" -> trace "random-walk" RandomWalk.run
      "bfs"    -> trace "bfs" BFS.run
      "dfs-gr" -> trace "dfs-greedy" DFSGreedy.run
      "montec" -> trace "montecarlo" MonteCarloTreeSearch.run
      _        -> trace prog MonteCarloTreeSearch.run
  }

downcase :: String -> String
downcase = map toLower
