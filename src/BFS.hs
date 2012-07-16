{-# OPTIONS_GHC -Wall #-}
module BFS
  ( run
  ) where

import Move
import GameState
import Sim
import SearchTree

-- Commandのリストは逆順なので注意
run :: (GameState -> [Command] -> IO ()) -> GameState -> IO ()
run check s0 = mapM_ (uncurry check) $ bfs $ [searchTree (gRemainingSteps s0 + 1) s0]

bfs :: [Tree] -> [(GameState, [Command])]
bfs ss = [x | Node x _ <- ss] ++ bfs [ch | Node _ children <- ss, (_, ch) <- children]
