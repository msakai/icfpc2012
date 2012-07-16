module SearchTree where

import Data.Maybe
import Move
import Sim

-- Commandのリストは逆順なので注意
data Tree = Node (GameState, [Command]) [(Command, Tree)]

searchTree :: GameState -> Tree
searchTree s0 = f s0 []
  where
    f s cmds = Node (s,cmds) children
      where
        children = [(c, f (step s c) (c:cmds)) | isNothing (gEnd s), c <- [minBound..maxBound]]
