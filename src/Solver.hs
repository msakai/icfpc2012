{-# LANGUAGE ScopedTypeVariables #-}
module Solver where

import Control.Exception
import Control.Monad
import Data.Array
import Data.IORef
import qualified System.Random as Rand

import Move
import Sim

run :: GameState -> IO ([Command], Int)
run s = do
  bestRef <- newIORef ([A], 0)  
  result <- try $ (run' bestRef s)
  case result of
    Right () -> return ()
    Left (e :: AsyncException) -> return ()
  (cmds, score) <- readIORef bestRef
  return (reverse cmds, score)

run' :: IORef ([Command], Int) -> GameState -> IO ()
run' bestRef s = forever $ walk s []
  where
    stepLim = rangeSize $ bounds $ gMap s

    walk :: GameState -> [Command] -> IO ()
    walk s cmds = do
      if gSteps s > stepLim
        then return ()
        else do
          check s cmds
          check (step s A) (A : cmds)
          c <- randomCommand
          walk (step s c) (c : cmds)

    check :: GameState -> [Command] -> IO ()
    check s cmds = do
      (bestCmds, bestScore) <- readIORef bestRef
      let score = gScore s
      if score > bestScore
        then writeIORef bestRef (cmds, score)
        else return ()

randomCommand :: IO Command
randomCommand = do
  let cmds = [L,R,U,D,W]
  i <- Rand.getStdRandom $ Rand.randomR (0, length cmds - 1)
  return $ cmds !! i
