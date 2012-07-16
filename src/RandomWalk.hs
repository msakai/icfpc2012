{-# OPTIONS_GHC -Wall #-}
module RandomWalk
  ( run
  ) where

import Prelude hiding (catch)
import Control.Monad
import Data.Maybe
import qualified System.Random as Rand

import Move
import GameState
import Sim

-- Commandのリストは逆順なので注意
run :: (GameState -> [Command] -> IO ()) -> GameState -> IO ()
run check s0 = forever $ walk s0 []
  where
    stepLim = gRemainingSteps s0

    walk :: GameState -> [Command] -> IO ()
    walk s cmds = do
      if gSteps s > stepLim
        then return ()
        else do
          if isJust (gEnd s)
            then return ()
            else do
              let cs = [L,R,U,D,W,S] -- Aは無条件にチェックするのでここで候補にはしない
                  children = [(step s c, c:cmds) | c <- cs, isMeaningfulCommand s c]
              forM_ children $ \(s,cmds) -> check s (A:cmds)
              unless (null children) $ do
                i <- Rand.getStdRandom $ Rand.randomR (0, length children - 1)
                uncurry walk (children !! i)
