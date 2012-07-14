module Move where

import Control.Monad
import Data.Array
import Data.List
import Data.Maybe
import Text.Printf

import Map

data Command
  = L
  | R
  | U
  | D
  | W
  | A
  deriving (Ord, Eq, Show, Read)

parseCommands :: String -> [Command]
parseCommands = map (read . return)

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

data EndingCondition
  = Winning
  | Losing
  | Abort
  deriving (Eq, Ord, Enum, Show)

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

isValidMove :: Map -> Pos -> Pos -> Bool
isValidMove m (x,y) (x',y') =
  case m ! (x',y') of
    Empty          -> True
    Earth          -> True
    Lambda         -> True
    OpenLambdaLift -> True
    Rock
      | x'==x+1 && y'==y &&
        inRange (bounds m) (x+2,y) &&  m ! (x+2,y) == Empty ->
          True
          -- Additionally, the Rock moves to (x+2,y).
      | x'==x-1 && y'==y &&
        inRange (bounds m) (x-2,y) &&  m ! (x-2,y) == Empty ->
          True
          -- Additionally, the Rock moves to (x−2,y).
    _ -> False

-- contest1.map
-- http://www.undecidable.org.uk/~edwin/cgi-bin/weblifter.cgi と結果が一致するのを確認
test_contest1 = printSim m act
  where
    m = parseMap'
        [ "######"
        , "#. *R#"
        , "#  \\.#"
        , "#\\ * #"
        , "L  .\\#"
        , "######"
        ]
    act = parseCommands "DLLLDDRRRLULLDL"

printSim :: Map -> [Command] -> IO ()
printSim m ms = do
  forM_ (simulate (initialState m) ms) $ \s -> do
    putStr $ showMap (gMap s)
    printf "Steps: %d; Score: %d; Lambda: %d\n" (gSteps s) (gScore s) (gLambda s)
    printf "End: %s\n" $ show (gEnd s)
    putStrLn ""

simulate :: GameState -> [Command] -> [GameState]
simulate s [] = [s]
simulate s (m:ms)
  | isJust (gEnd s) = [s]
  | otherwise = s : simulate (step s m) ms

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
