{-# LANGUAGE BangPatterns #-}
module SearchTree where

import Data.Maybe
import Move
import Sim

data Tree = Node GameState [(Command, Tree)]

searchTree :: Int -> GameState -> Tree
searchTree depthLim s0 = f 0 s0
  where
    f !d s = Node s children
      where
        children = [ (c, f (d+1) s')
                   | depthLim > d, isNothing (gEnd s)
                   , c <- [minBound..maxBound]
                   , let s' = step s c
                   , isMeaningfulCommand s c
                   ]
