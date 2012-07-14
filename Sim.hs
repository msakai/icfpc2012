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
  , stepN
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
import Data.Monoid
import qualified Data.Foldable as F
import qualified Data.Sequence as Seq
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
  { gPos    :: !Pos -- ^ ロボットの現在位置
  , gMap    :: !Map -- ^ 地図
  , gScore  :: !Int -- ^ スコア
  , gLambda :: !Int -- ^ 獲得したラムダの数
  , gLambdaLeft :: [Pos]  -- ^ 未回収のラムダの位置のリスト
  , gSteps  :: !Int -- ^ 実行ステップ数
  , gEnd    :: Maybe EndingCondition -- ^ 終了条件

  , gWater      :: !Int -- ^ 現在の水位
  , gFlooding   :: !Int -- ^ 水位上昇ペース
  , gWaterproof :: !Int -- ^ ロボットが水中にいて大丈夫な時間
  , gUnderwater :: !Int -- ^ 現在ロボットが水面下にいる継続時間
  }
  deriving (Eq, Show)

initialState :: Map -> (Int,Int,Int) -> GameState
initialState m (water,flooding,waterproof)
  = GameState
  { gPos    = head [i | (i,Robot) <- assocs m]
  , gMap    = m
  , gScore  = 0
  , gLambda = 0
  , gLambdaLeft = map fst $ filter ((Lambda ==). snd) $ assocs m
  , gSteps  = 0
  , gEnd    = Nothing
  , gWater      = water
  , gFlooding   = flooding
  , gWaterproof = waterproof
  , gUnderwater = 0
  }

initialStateFromString :: String -> GameState
initialStateFromString s = initialState m (water, flooding, waterproof)
  where
    ls = lines s
    (ls1,ls2) = break ([]==) ls
    m    = parseMap' ls1
    meta = parseMetadata' ls2
    water      = fromMaybe 0  $ lookup "Water" meta
    flooding   = fromMaybe 0  $ lookup "Flooding" meta
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
    f xy@(x',y') = 
      case m ! (x',y') of
        Empty          -> s'
        Earth          -> s'
        Lambda         -> s'{ gScore = gScore s' + 25
                            , gLambda = gLambda s' + 1 
                            , gLambdaLeft = delete xy (gLambdaLeft s')
                            }
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
  | otherwise = s''
  where
    s' = move cmd s
    s'' = updateWater s (updateMap s s')

updateMap :: GameState -> GameState -> GameState
updateMap orig new =
  new{ gMap = m2
     , gEnd =
         -- XXX: 最期のmapのupdateで配置されたということをアドホックに表現している
         if gMap new ! (x,y+1) == Rock && gMap orig ! (x,y+1) /= Rock
           then Just Losing
           else gEnd new
     } 
  where
    (x,y) = gPos new
    m2 = update (gMap new)

-- XXX: underwater の判定タイミングとかよく分かっていない
updateWater :: GameState -> GameState -> GameState
updateWater orig new =
  new{ gWater = water
     , gUnderwater = underwater       
     , gEnd =
         if underwater > gWaterproof new
           then Just Losing
           else gEnd new
     }
  where
    water =
      if gFlooding new > 0 && gSteps new `mod` gFlooding orig == 0
        then gWater new + 1
        else gWater new
    underwater =
      if isUnderwater orig -- ???
        then gUnderwater new + 1
        else 0

stepN :: GameState -> [Command] -> GameState
stepN s cmds = foldl' step s cmds

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
interactiveSim s0 = go (s0,Seq.empty) []
  where
    go curr@(s,_) undoBuf = do
      printState s
      putStrLn ""
      prompt curr undoBuf
    prompt curr@(s,trace) undoBuf = do
      putStr "> "
      hFlush stdout
      l <- liftM (filter (not . isSpace)) getLine
      case l of
        ":quit" -> return ()
        ":dump" -> do
          putStrLn $ showCommands (F.toList trace)
          prompt (s,trace) undoBuf
        ":undo" -> do
          case undoBuf of
            [] -> do
              putStrLn "empty undo buffer"
              go (s,trace) undoBuf
            (old:undoBuf') -> go old undoBuf'
        ":reset" -> do
          case undoBuf of
            [] -> go (s,trace) undoBuf
            _ -> go (last undoBuf) []
        _ | isNothing (gEnd s) && all (`elem` "LRUDWA") (map toUpper l) -> do
          let cs = parseCommands (map toUpper l)
          go (stepN s cs, trace <> Seq.fromList cs) (curr : undoBuf)
        _ -> do
          putStrLn "parse error"
          prompt curr undoBuf

{--------------------------------------------------------------------
  Tests
--------------------------------------------------------------------}

-- contest1.map
-- http://www.undecidable.org.uk/~edwin/cgi-bin/weblifter.cgi と結果が一致するのを確認
test_contest1 :: IO ()
test_contest1 = printSim (initialState contest1 (0,0,10)) act
  where
    act = parseCommands "DLLLDDRRRLULLDL"

test_contest2 :: IO ()
test_contest2 = printSim (initialState contest2 (0,0,10)) act
  where
    act = parseCommands "RRUDRRULURULLLLDDDL"
