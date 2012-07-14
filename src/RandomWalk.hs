{-# OPTIONS_GHC -Wall #-}
module RandomWalk
  ( run
  ) where

import Control.Monad
import Data.Array
import Data.IORef
import System.IO
import qualified System.Random as Rand

import Move
import Sim

run :: IORef ([Command], Int) -> GameState -> IO ()
run bestRef s0 = forever $ walk s0 []
  where
    stepLim = rangeSize $ bounds $ gMap s0

    walk :: GameState -> [Command] -> IO ()
    walk s cmds = do
      if gSteps s > stepLim
        then return ()
        else do
          check (step s A) (A : cmds)
          c <- randomCommand
          walk (step s c) (c : cmds)

    check :: GameState -> [Command] -> IO ()
    check s cmds = do
      (_, bestScore) <- readIORef bestRef
      let score = gScore s
      if score > bestScore
        then do
          hPutStrLn stderr $ "best score = " ++ show score
          hPutStrLn stderr $ "commands = " ++ showCommands (reverse cmds)
          writeIORef bestRef (cmds, score)
        else return ()

randomCommand :: IO Command
randomCommand = do
  let cmds = [L,R,U,D,W] -- Aは無条件にチェックするのでここで候補にはしない
  i <- Rand.getStdRandom $ Rand.randomR (0, length cmds - 1)
  return $ cmds !! i
