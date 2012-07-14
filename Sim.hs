{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
module Sim
  (
  -- * The GameState type
    GameState (..)
  , initialState
  , initialStateFromString
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
import Data.List
import Data.Maybe
import System.IO
import Text.Printf

import Map
import Move
import Metadata

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

  , gWater      :: !Int
  , gFlooding   :: !Int
  , gWaterproof :: !Int
  , gUnderwater :: !Int
  }
  deriving (Eq, Show)

initialState :: Map -> (Int,Int,Int) -> GameState
initialState m (water,flooding,waterproof)
  = GameState
  { gPos    = head [i | (i,Robot) <- assocs m]
  , gMap    = m
  , gScore  = 0
  , gLambda = 0
  , gSteps  = 0
  , gEnd    = Nothing

  , gWater      = water
  , gFlooding   = flooding
  , gWaterproof = waterproof
  , gUnderwater = 0          -- ロボットが水面下にいる時間
  }

initialStateFromString :: String -> GameState
initialStateFromString s = initialState m (water, flooding, waterproof)
  where
    ls = lines s
    (ls1,ls2) = break ([]==) ls
    m    = parseMap' ls1
    meta = parseMetadata' ls2
    water      = fromMaybe 0 $ lookup "Water" meta
    flooding   = fromMaybe 0 $ lookup "Flooding" meta
    waterproof = fromMaybe 10 $ lookup "Waterproof" meta                  

printState :: GameState -> IO ()
printState s = do
  putStr $ showMap (gMap s)
  printf "Steps: %d; Score: %d; Lambda: %d\n" (gSteps s) (gScore s) (gLambda s)
  printf "Water: %d; Flooding: %d; Waterproof: %d; Underwater: %d\n"
    (gWater s) (gFlooding s) (gWaterproof s) (gUnderwater s)
  case gEnd s of
    Nothing -> return ()
    Just w -> printf "End: %s\n" $ show w

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
    W -> s{ gScore = gScore s - 1, gSteps = gSteps s + 1 }
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
        _ -> -- invalid
          s{ gScore = gScore s - 1
           , gSteps = gSteps s + 1
           } 
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
  | gWaterproof s'' >= 0 && gUnderwater s'' > gWaterproof s'' =
      -- XXX: underwater の判定タイミングよく分かっていない
      s''{ gEnd = Just Losing }
  | otherwise = s''
  where
    s' = move cmd s
    s'' = s'{ gMap = update (gMap s')
            , gWater =
                if gFlooding s' > 0 && gSteps s' `mod` gFlooding s' == 0
                  then gWater s' + 1
                  else gWater s'
            , gUnderwater = -- XXX: underwater の判定タイミングよく分かっていない
                if isUnderwater s
                  then gUnderwater s' + 1
                  else 0
            }
    (x,y) = gPos s''

isUnderwater :: GameState -> Bool
isUnderwater s = gWater s >= y
  where
    (_,y) = gPos s

simulate :: GameState -> [Command] -> [GameState]
simulate s [] = [s]
simulate s (m:ms)
  | isJust (gEnd s) = [s]
  | otherwise = s : simulate (step s m) ms

printSim :: GameState -> [Command] -> IO ()
printSim s ms = do
  forM_ (simulate s ms) $ \s' -> do
    printState s'
    putStrLn ""

interactiveSim :: GameState -> IO ()
interactiveSim s0 = go s0 []
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
        _ | all (`elem` "LRUDWA") l -> do
          let cs = parseCommands l
          go (foldl' step s cs) (reverse cs ++ trace)
        _ -> do
          putStrLn "parse error"
          prompt s trace

{--------------------------------------------------------------------
  Tests
--------------------------------------------------------------------}

-- contest1.map
-- http://www.undecidable.org.uk/~edwin/cgi-bin/weblifter.cgi と結果が一致するのを確認
test_contest1 :: IO ()
test_contest1 = printSim (initialState contest1 (-1,-1,-1)) act
  where
    act = parseCommands "DLLLDDRRRLULLDL"

test_contest2 :: IO ()
test_contest2 = printSim (initialState contest2 (-1,-1,-1)) act
  where
    act = parseCommands "RRUDRRULURULLLLDDDL"
