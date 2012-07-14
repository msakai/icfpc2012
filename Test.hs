{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Control.Monad
import Data.List
import Test.HUnit hiding (Test)
import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.TH
import Test.Framework.Providers.HUnit

import Map
import Move
import Sim

case_contest2_map = do
  gEnd s   @?= Just Winning
  gScore s @?= 281
  where
    act = parseCommands "RRUDRRULURULLLLDDDL"
    s0  = initialState contest2 (0,0,10)
    s   = stepN s0 act

------------------------------------------------------------------------
-- Test harness

main :: IO ()
main = $(defaultMainGenerator)
