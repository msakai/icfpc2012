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
    s0  = initialStateFromString contest1
    s   = stepN s0 act

case_contest1_map_losing = do
  gEnd s   @?= Just Losing
  gScore s @?= -2
  where
    act = parseCommands "DD"
    s0  = initialStateFromString contest1
    s   = stepN s0 act

contest1 =  unlines
  [ "######"
  , "#. *R#"
  , "#  \\.#"
  , "#\\ * #"
  , "L  .\\#"
  , "######"
  ]

case_contest2_map = do
  gEnd s   @?= Just Winning
  gScore s @?= 281
  where
    act = parseCommands "RRUDRRULURULLLLDDDL"
    s0  = initialStateFromString contest2
    s   = stepN s0 act

contest2 = unlines
  [ "#######"
  , "#..***#"
  , "#..\\\\\\#"
  , "#...**#"
  , "#.*.*\\#"
  , "LR....#"
  , "#######"
  ]

case_contest3_map = do
  gEnd s   @?= Just Winning
  gScore s @?= 275
  where
    act = parseCommands "LDDDRRRRDDLLLLLDURRRUURRR"
    s0  = initialStateFromString contest3
    s   = stepN s0 act

contest3 = unlines
  [ "########"
  , "#..R...#"
  , "#..*...#"
  , "#..#...#"
  , "#.\\.\\..L"
  , "####**.#"
  , "#\\.....#"
  , "#\\..* .#"
  , "########"
  ]

case_contest4_map = do
  gEnd s   @?= Just Winning
  gScore s @?= 575
  where
    act = parseCommands "DUURDDDDRDRRRLUUURUUULDRR"
    s0  = initialStateFromString contest4
    s   = stepN s0 act

contest4 = unlines
  [ "#########"
  , "#.*..#\\.#"
  , "#.\\..#\\.L"
  , "#.R .##.#"
  , "#.\\  ...#"
  , "#..\\  ..#"
  , "#...\\  ##"
  , "#....\\ \\#"
  , "#########"
  ]

case_contest7_map = do
  gEnd s   @?= Just Winning
  gScore s @?= 867
  where
    act = parseCommands "RDRRRDDLLLDDLLRRUURRRRDDRRRLLLULL"
    s0  = initialStateFromString contest7
    s   = stepN s0 act

contest7 = unlines
  [ "    #######"
  , "    ##    *#"
  , "     ##R  *##"
  , "      ##\\\\\\\\##"
  , "       ##....##"
  , "      ##..\\ . ##"
  , "     ## . L .  ##"
  , "    ##\\\\\\# #\\\\\\\\##"
  , "   ######   #######"
  ]

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

case_flood5_map_bug1 = do
  gEnd s1   @?= Just Losing
  gScore s1 @?= 108
  where
    act = parseCommands "DRDRDRDWWUUDDRRL"
    s0  = initialStateFromString flood5
    s1  = stepN s0 (act ++ [U])

case_flood5_map_bug2 = do
  gEnd s1   @?= Just Losing
  gScore s1 @?= 16
  where
    act = parseCommands "DDDDWWWWW"
    s0  = initialStateFromString flood5
    s1  = stepN s0 act

flood5 = unlines
  [ "#########"
  , "#.*..#\\.#"
  , "#.\\..#\\.L"
  , "#.R .##.#"
  , "#.\\  ...#"
  , "#..\\  ..#"
  , "#...\\  ##"
  , "#....\\ \\#"
  , "#########"
  , ""
  , "Water 2"
  , "Flooding 11"
  , "Waterproof 5"
  ]

case_trampoline1_map = do
  gEnd s1   @?= Just Winning
  gScore s1 @?= 419
  where
    act = parseCommands "RRLDDRRRUULLLLLDLLLLURRRURRRDDD"
    s0  = initialStateFromString trampoline1
    s1  = stepN s0 act

case_cannot_move_onto_target = do
  gPos s1 @?= (6,4)
  where
    act = parseCommands "DD"
    s0  = initialStateFromString trampoline1
    s1  = stepN s0 act

case_all_Trampolines_with_the_same_Target_are_removed = do
  gMap s1 @?= m
  where
    act = parseCommands "DLL"
    s0  = initialStateFromString trampoline1
    s1  = stepN s0 act
    m   = parseMap'
      [ "############     "
      , "#.. . .. ..#     "
      , "#..*  ..*..######"
      , "#....2.. ..#\\\\\\C#"  -- web validator では何故かCがTに、2がtになってる
      , "#......* *.#\\\\\\R#"
      , "########L########"
      ]

trampoline1 = unlines
  [ "############"
  , "#..*.R..*..#"
  , "#..A....B..######"
  , "#....2.. ..#\\\\\\C#"
  , "#......* *.#\\\\\\1#"
  , "########L########"
  , ""
  , "Trampoline A targets 1"
  , "Trampoline B targets 1"
  , "Trampoline C targets 2"
  ]

case_razor = do
  gMap s @?= m
  gScore s @?= 34
  where
    act = parseCommands "RRLDLLDURRRDDDWSA"
    s0  = initialStateFromString beard1 
    s   = stepN s0 act
    m   = parseMap'
      [ "##########"
      , "#*   \\\\\\\\#"
      , "#.       #"
      , "#    ..*\\#"
      , "#  * ..*!#"
      , "####   # #"
      , "#\\\\.R. # L"
      , "#\\\\. ... #"
      , "#\\\\.WW   #"
      , "##########"
      ]

beard1 = unlines 
  [ "##########"
  , "#**  \\\\\\\\#"
  , "#.R..    #"
  , "# \\  ..*\\#"
  , "#!   ..*!#"
  , "####   # #"
  , "#\\\\... # L"
  , "#\\\\.W... #"
  , "#\\\\.     #"
  , "##########"
  , ""
  , "Growth 15"
  , "Razors 0"
  ]

case_horock = do
  gEnd s @?= Just Losing
  gMap s @?= m
  gScore s @?= 19
  where
    act = parseCommands "RRDDDD"
    s0  = initialStateFromString horock1
    s   = stepN s0 act
    m   = parseMap'
      [ "#####L######"
      , "#....   ..A#"    -- web validator だと A が何故か T になる
      , "#...... ...#"
      , "#\\.  #   ..#"
      , "#\\...#*\\ ###"
      , "#\\.....R...#"
      , "#\\..*.@....#"
      , "####@@.  ..#"
      , "#....#.. ###"
      , "#....#..**.#"
      , "#.1..#  .\\\\#"  -- web validator だと 1 が何故か t になる
      , "############"
      ]

horock1 = unlines
  [ "#####L######"
  , "#....R....A#"
  , "#..........#"
  , "#\\.  #@\\ ..#"
  , "#\\...#*  ###"
  , "#\\.........#"
  , "#\\..*.@....#"
  , "####@@.  ..#"
  , "#....#.. ###"
  , "#....#..**.#"
  , "#.1..#  .\\\\#"
  , "############"
  , ""
  , "Trampoline A targets 1"
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
