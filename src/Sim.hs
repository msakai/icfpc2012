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

import Map
import Move
import GameState

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
    W -> s1
    S | gRazors s1 > 0 -> s1{ gMap = applyRazor (x,y) m, gRazors = gRazors s1 - 1 }
      | otherwise      -> s1
    A -> s{ gSteps = gSteps s + 1, gEnd = Just Abort, gScore = gScore s + gLambda s * 25 }
  where
    s1 = s{ gSteps = gSteps s + 1, gScore = gScore s - 1 }
    f xy@(x',y') = 
      case m ! (x',y') of
        Empty          -> s'
        Earth          -> s'
        Lambda         -> s'{ gScore = gScore s' + 25
                            , gLambda = gLambda s' + 1 
                            , gLambdaLeft = delete xy (gLambdaLeft s')
                            }
        Razor          -> s'{ gRazors = gRazors s' + 1 }
        OpenLambdaLift -> s'{ gEnd = Just Winning, gScore = gScore s' + gLambda s' * 50 }
        c
          | isRock c && x'==x+1 && y'==y &&
            inRange (bounds m) (x+2,y) &&  m ! (x+2,y) == Empty ->
              -- Additionally, the Rock moves to (x+2,y).
              s'{ gMap = gMap s' // [((x+2,y), c)] }
          | isRock c && x'==x-1 && y'==y &&
            inRange (bounds m) (x-2,y) &&  m ! (x-2,y) == Empty ->
              -- Additionally, the Rock moves to (x-2,y).
              s'{ gMap = gMap s' // [((x-2,y), c)] }
        Trampoline c   -> s1 { gMap = wm
                             , gPos = to }
                           where
                             to = fromJust $ lookup c $ gTrampoline s
                             Target c' = getCell m to
                             froms = fromJust $ lookup c' $ gTarget s 
                             wm = (m //) $ ((x,y),Empty) 
                                         : (to,Robot) 
                                         : zip froms (repeat Empty)
        _ -> s1 -- invalid case

      where
        m' = m // ([((x',y'), Robot) | m ! (x',y') /= OpenLambdaLift] ++ [((x,y), Empty)])
        s' = s1{ gMap = m'
               , gPos = (x',y')
               }

applyRazor :: Pos -> Map -> Map
applyRazor (x,y) m = m // xs
  where
    xs = do
      dx<-[-1..1]
      dy<-[-1..1]
      let x' = x+dx
          y' = y+dy
      guard $ (x',y') /= (x,y)
      guard $ m ! (x',y') == Beard
      return ((x',y'), Empty)

-- TODO: ヒゲの更新処理
step :: GameState -> Command -> GameState
step s cmd
  | isJust (gEnd s)  = s  -- 終了済みなら何もしない (エラーにすべき?)
  | isJust (gEnd s') = s' -- Win/Abortの場合にはその後のmapのupdateとLoosingの判定はしなくて良いようだ
  | otherwise = updateWater $ updateMap s'
  where
    s' = move cmd s

updateMap :: GameState -> GameState
updateMap s = case m2 of
  Left  m' -> s { gMap = m', gEnd = Just Losing }
  Right m' -> s { gMap = m' } 
  where
    m2 = update (gMap s) lambdaRemaining growBeard
    growBeard = gGrowth s > 0 && gSteps s `mod` gGrowth s == 0
    lambdaRemaining = gReqLambda s > gLambda s

{-|
処理順:

(1) move

(2) update of map & check if the robot is underwater

(3) update of water level

根拠:

* 「Given a setting of Waterproof n, the robot may be underwater after n
  consecutive /map updates/. If after the next update it is still underwater,
  it becomes inoperative.」とあるので、move後の位置で浸水しているときの map update
  の回数を数えれば良い。

* 「/After/ every 10 steps in the /map update phase/, given by Flooding,
  the water level will rise by 1.」とあるので水位の更新は、map の update 後
-}
updateWater :: GameState -> GameState
updateWater s =
    s{ gWater = water
     , gUnderwater = underwater       
     , gEnd =
         if underwater > gWaterproof s
           then Just Losing
           else gEnd s
     }
  where
    flooding = gFlooding s
    water =
      if flooding > 0 && gSteps s `mod` flooding == 0
        then gWater s + 1
        else gWater s
    underwater =
      if isUnderwater s
        then gUnderwater s + 1
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
        ":q"    -> return ()
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
        _ | isNothing (gEnd s) && all (`elem` "LRUDWAS") (map toUpper l) -> do
          let cs = parseCommands (map toUpper l)
          go (stepN s cs, trace <> Seq.fromList cs) (curr : undoBuf)
        _ -> do
          putStrLn "parse error"
          prompt curr undoBuf
