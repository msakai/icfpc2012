{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Solver where

import Control.Exception
import Data.IORef

import Move
import Sim
import qualified RandomWalk

run :: GameState -> IO ([Command], Int)
run s = do
  bestRef <- newIORef ([A], 0)  
  result <- try $ RandomWalk.run bestRef s
  case result of
    Right () -> return ()
    Left (_ :: AsyncException) -> return ()
  (cmds, score) <- readIORef bestRef
  return (reverse cmds, score)
