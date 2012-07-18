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
run :: (GameState -> IO ()) -> GameState -> IO ()
run check s0 = forever $ walk s0
  where
    stepLim = gRemainingSteps s0

    walk :: GameState -> IO ()
    walk s = do
      if gSteps s > stepLim
        then return ()
        else do
          if isJust (gEnd s)
            then return ()
            else do
              let cs = [L,R,U,D,W,S] -- Aは無条件にチェックするのでここで候補にはしない
                  children = [step s c | c <- cs, isMeaningfulCommand s c]
              forM_ children check
              if (or [gEnd s' == Just Winning| s' <- children])
                then return ()
                else do
                  let children2 = [s' | s' <- children, isNothing (gEnd s')]
                  unless (null children2) $ do
                    i <- Rand.getStdRandom $ Rand.randomR (0, length children2 - 1)
                    walk (children2 !! i)
