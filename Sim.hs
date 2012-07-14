{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
module Sim
  (
  -- * The GameState type
    GameState (..)
  , initialState
  , printState

  -- * simulation
  , step
  , simulate
  , interactiveSim

  -- * utilities
  , printSim  
  ) where

import Control.Monad
import Data.Array
import Data.Char
import Data.Maybe
import System.IO
import Text.Printf

import Map
import Move

{--------------------------------------------------------------------
  GameState
--------------------------------------------------------------------}

data GameState
  = GameState
  { gPos    :: !Pos
  , gMap    :: !Map
  , gScore  :: !Int
  , gLambda :: !Int
  , gSteps  :: !Int
  , gEnd    :: Maybe EndingCondition
  }
  deriving (Eq, Show)

initialState :: Map -> GameState
initialState m
  = GameState
  { gPos    = head [i | (i,Robot) <- assocs m]
  , gMap    = m
  , gScore  = 0
  , gLambda = 0
  , gSteps  = 0
  , gEnd    = Nothing
  }

printState :: GameState -> IO ()
printState s = do
  putStr $ showMap (gMap s)
  printf "Steps: %d; Score: %d; Lambda: %d\n" (gSteps s) (gScore s) (gLambda s)
  printf "End: %s\n" $ show (gEnd s)

{--------------------------------------------------------------------
  Simulation
--------------------------------------------------------------------}

move :: Command -> GameState -> GameState
move cmd s@GameState{ gMap = m, gPos = (x,y) } =
  case cmd of
    L -> f (x-1, y)
    R -> f (x+1, y)
    U -> f (x, y+1)
    D -> f (x, y-1)
    W -> s{ gSteps = gSteps s + 1 }
    A -> s{ gSteps = gSteps s + 1, gEnd = Just Abort, gScore = gScore s + gLambda s * 25 }
  where
    f (x',y') = 
      case m ! (x',y') of
        Empty          -> s'
        Earth          -> s'
        Lambda         -> s'{ gScore = gScore s' + 25, gLambda = gLambda s' + 1 }
        OpenLambdaLift -> s'{ gEnd = Just Winning, gScore = gScore s' + gLambda s' * 50 }
        Rock
          | x'==x+1 && y'==y &&
            inRange (bounds m) (x+2,y) &&  m ! (x+2,y) == Empty ->
              -- Additionally, the Rock moves to (x+2,y).
              s'{ gMap = gMap s' // [((x+2,y), Rock)] }
          | x'==x-1 && y'==y &&
            inRange (bounds m) (x-2,y) &&  m ! (x-2,y) == Empty ->
              -- Additionally, the Rock moves to (x-2,y).
              s'{ gMap = gMap s' // [((x-2,y), Rock)] }
        _ -> s{ gSteps = gSteps s + 1 } -- invalid
      where
        m' = m // ([((x',y'), Robot) | m ! (x',y') /= OpenLambdaLift] ++ [((x,y), Empty)])
        s' = s{ gMap = m'
              , gPos = (x',y')
              , gScore = gScore s - 1
              , gSteps = gSteps s + 1
              }

step :: GameState -> Command -> GameState
step s cmd
  | isJust (gEnd s') = s' -- Win/Abortの場合にはその後のmapのupdateとLoosingの判定はしなくて良いようだ
  | gMap s'' ! (x,y+1) == Rock && gMap s' ! (x,y+1) /= Rock =
      -- XXX: 最期のmapのupdateで配置されたということをアドホックに表現している
      s''{ gEnd = Just Losing }
  | otherwise = s''
  where
    s' = move cmd s
    s'' = s'{ gMap = update (gMap s') }
    (x,y) = gPos s''

simulate :: GameState -> [Command] -> [GameState]
simulate s [] = [s]
simulate s (m:ms)
  | isJust (gEnd s) = [s]
  | otherwise = s : simulate (step s m) ms

printSim :: Map -> [Command] -> IO ()
printSim m ms = do
  forM_ (simulate (initialState m) ms) $ \s -> do
    printState s
    putStrLn ""

interactiveSim :: Map -> IO ()
interactiveSim m = go (initialState m) []
  where
    go s trace = do
      printState s
      putStrLn ""
      if isJust (gEnd s)
        then do
          putStrLn $ "trace: " ++ showCommands (reverse trace)
          return ()
        else prompt s trace
    prompt s trace = do
      putStr "> "
      hFlush stdout
      l <- liftM (filter (not . isSpace)) getLine
      case l of
        ":quit" -> return ()
        ":command" -> do
          putStrLn $ "command: " ++ showCommands (reverse trace)
          prompt s trace
        [c] | c `elem` "LRUDWA" -> do
          let cmd = read [c]
          go (step s cmd) (cmd : trace)
        _ -> do
          putStrLn "parse error"
          prompt s trace

{--------------------------------------------------------------------
  Tests
--------------------------------------------------------------------}

-- contest1.map
-- http://www.undecidable.org.uk/~edwin/cgi-bin/weblifter.cgi と結果が一致するのを確認
test_contest1 :: IO ()
test_contest1 = printSim contest1 act
  where
    act = parseCommands "DLLLDDRRRLULLDL"

test_contest2 :: IO ()
test_contest2 = printSim contest2 act
  where
    act = parseCommands "RRUDRRULURULLLLDDDL"
