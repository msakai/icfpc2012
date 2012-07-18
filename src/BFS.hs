{-# OPTIONS_GHC -Wall #-}
module BFS
  ( run
  ) where

import Move
import GameState
import Sim
import SearchTree

run :: (GameState -> IO ()) -> GameState -> IO ()
run check s0 = mapM_ check $ bfs $ [searchTree (gRemainingSteps s0 + 1) s0]

bfs :: [Tree] -> [GameState]
bfs ss = [x | Node x _ <- ss] ++ bfs [ch | Node _ children <- ss, (_, ch) <- children]
