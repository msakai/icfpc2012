{-# OPTIONS_GHC -Wall #-}
module DFSGreedy
  ( run
  ) where

import Data.Array
import Data.List
import Data.Ord (comparing)
import qualified Data.Sequence as Seq

import Map
import GameState
import SearchTree

run :: (GameState -> IO ()) -> GameState -> IO ()
run check s0 = mapM_ check $ greedy $ [searchTree (gRemainingSteps s0 + 1) s0]

greedy :: [Tree] -> [GameState]
greedy ts = go [(t, pickTarget s, Seq.singleton (gPos s)) | t@(Node s _) <- ts]

go :: [(Tree, Pos, Seq.Seq Pos)] -> [GameState]
go [] = []
go ((Node s children, p, hist) : ts) = s : go (ts' ++ ts)
  where
    p' | gPos s == p = pickTarget s
       | otherwise   = p
    ts' = sortBy (comparing g) [(ch, p', gPos s' Seq.<| Seq.take 10 hist) | (_, ch@(Node s' _)) <- children]
    g (Node s' _, _, _) = (not (Seq.null (Seq.filter (gPos s' ==) hist)), manhattan (gPos s') p')

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
