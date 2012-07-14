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

case_contest1_map = do
  gEnd s   @?= Just Winning
  gScore s @?= 210
  where
    act = parseCommands "DLLLDDRRRLULLDL"
    s0  = initialState contest1 (0,0,10)
    s   = stepN s0 act

case_contest1_loose_map = do
  gEnd s   @?= Just Losing
  -- gScore s @?= ???
  where
    act = parseCommands "DD"
    s0  = initialState contest1 (0,0,10)
    s   = stepN s0 act

case_contest2_map = do
  gEnd s   @?= Just Winning
  gScore s @?= 281
  where
    act = parseCommands "RRUDRRULURULLLLDDDL"
    s0  = initialState contest2 (0,0,10)
    s   = stepN s0 act

case_flood2_map = do
  gEnd s1   @?= Nothing
  gScore s1 @?= -13
  gEnd s2   @?= Just Losing
  gScore s2 @?= -14
  where
    act = parseCommands "WWWWWWWWWWWWW"
    s0  = initialStateFromString flood2
    s1  = stepN s0 act
    s2  = step s1 W

flood2 = unlines
  [ "#######"
  , "#..***#"
  , "#..\\\\\\#"
  , "#...**#"
  , "#.*.*\\#"
  , "LR....#"
  , "#######"
  , ""
  , "Flooding 5"
  , "Waterproof 3"
  ]

case_example = do
  gEnd s   @?= Just Winning
  where
    act = parseCommands "DDDLLLLLLURRRRRRRRRRRRDDDDDDDLLLLLLLLLLLDDDRRRRRRRRRRRD"
    s0  = initialStateFromString example
    s   = stepN s0 act

example = unlines
  [ "###############"
  , "#***...R......#"
  , "#***... ...*..#"
  , "#\\\\\\... ..\\\\\\.#"
  , "#...... ...*..#"
  , "#..     .. ...#"
  , "#.... .... ...#"
  , "#.... .... ...#"
  , "#.. .       ..#"
  , "#..*. .. .....#"
  , "#.... .. .....#"
  , "#.\\.. .......*#"
  , "#.............#"
  , "#.........   .#"
  , "#############L#"
  ]

------------------------------------------------------------------------
-- Test harness

main :: IO ()
main = $(defaultMainGenerator)
