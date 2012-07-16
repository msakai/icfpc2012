{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Solver where

import Control.Exception
import Control.Monad
import Data.IORef
import System.IO

import Move
import Sim
import qualified RandomWalk

run :: GameState -> IO ([Command], Int)
run s0 = do
  bestRef <- newIORef ([A], 0)
  let -- Commandのリストは逆順なので注意
      check :: GameState -> [Command] -> IO ()
      check s cmds = do
        (_, bestScore) <- readIORef bestRef
        let score = gScore s
        when (score > bestScore) $ do
          hPutStrLn stderr $ "best score = " ++ show score
          hPutStrLn stderr $ "commands = " ++ showCommands (reverse cmds)
          writeIORef bestRef (cmds, score)

  result <- try $ RandomWalk.run check s0
  -- XXX
  case result of
    Right () -> return ()
    Left (_ :: AsyncException) -> return ()

  (cmds, score) <- readIORef bestRef
  return (reverse cmds, score)
