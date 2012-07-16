{-# OPTIONS_GHC -Wall #-}
module DFSGreedy
  ( run
  ) where

import Data.Array
import Data.List
import Data.Ord (comparing)

import Map
import Move
import GameState
import SearchTree

-- Commandのリストは逆順なので注意
run :: (GameState -> [Command] -> IO ()) -> GameState -> IO ()
run check s0 = mapM_ (uncurry check) $ greedy $ [searchTree (gRemainingSteps s0 + 1) s0]

greedy :: [Tree] -> [(GameState, [Command])]
greedy ts = go [(t, pickTarget s) | t@(Node (s,_) _) <- ts]

go :: [(Tree, Pos)] -> [(GameState, [Command])]
go [] = []
go ((Node (s,cmds) children, p) : ts) = (s,A:cmds) : go (ts' ++ ts)
  where
    p' | gPos s == p = pickTarget s
       | otherwise   = p
    ts' = sortBy (comparing g) [(ch, p') | (_, ch) <- children]
    g (Node (s',_) _, _) = manhattan (gPos s') p'

pickTarget :: GameState -> Pos
pickTarget s
  | gLambda s == gReqLambda s = head [p | (p, OpenLambdaLift) <- assocs (gMap s)]
  | otherwise = 
      case gLambdaLeft s of
        [] ->
          -- update処理で必要なラムダが消されている可能性にがあるため
          head [p | (p, OpenLambdaLift) <- assocs (gMap s)]
        ps  ->
          fst $ minimumBy (comparing snd) [(p, manhattan p (gPos s)) | p <- ps]
