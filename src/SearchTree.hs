{-# LANGUAGE BangPatterns #-}
module SearchTree where

import Data.Maybe
import Move
import Sim

-- Commandのリストは逆順なので注意
data Tree = Node (GameState, [Command]) [(Command, Tree)]

searchTree :: Int -> GameState -> Tree
searchTree depthLim s0 = f 0 s0 []
  where
    f !d s cmds = Node (s,cmds) children
      where
        children = [(c, f (d+1) (step s c) (c:cmds)) | depthLim > d, isNothing (gEnd s), c <- [minBound..maxBound]]
