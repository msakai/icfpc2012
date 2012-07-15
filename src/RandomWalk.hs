{-# OPTIONS_GHC -Wall #-}
module RandomWalk
  ( run
  ) where

import Prelude hiding (catch)
import Control.Monad
import Data.Array
import Data.Maybe
import qualified System.Random as Rand

import Move
import Sim

run :: (GameState -> [Command] -> IO ()) -> GameState -> IO ()
run check s0 = forever $ walk s0 []
  where
    stepLim = rangeSize $ bounds $ gMap s0

    walk :: GameState -> [Command] -> IO ()
    walk s cmds = do
      if gSteps s > stepLim
        then return ()
        else do
          check (step s A) (A : cmds)
          if isJust (gEnd s)
            then return ()
            else do
              c <- randomCommand
              walk (step s c) (c : cmds)

randomCommand :: IO Command
randomCommand = do
  let cmds = [L,R,U,D,W,S] -- Aは無条件にチェックするのでここで候補にはしない
  i <- Rand.getStdRandom $ Rand.randomR (0, length cmds - 1)
  return $ cmds !! i
