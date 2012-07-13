module Move where

import Control.Monad
import Data.Array

import Map

data Move
  = L
  | R
  | U
  | D
  | W
  | A
  deriving (Ord, Eq, Show)

data GameState
  = GameState
  { gPos    :: !Pos
  , gMap    :: !Map
  , gScore  :: !Int
  , gLambda :: !Int
  , gSteps  :: !Int
  }
  deriving (Eq, Show)

move :: Move -> GameState -> Maybe GameState
move move s@GameState{ gMap = m, gPos = (x,y) } =
  case move of
    L -> f (x-1, y)
    R -> f (x+1, y)
    U -> f (x, y+1)
    D -> f (x, y-1)
    W -> Just s
    A -> Nothing
  where
    f (x',y') = 
      case m ! (x',y') of
        Empty          -> Just $ s'
        Earth          -> Just $ s'
        Lambda         -> Just $ s'{ gScore = gScore s' + 25 }
        OpenLambdaLift -> Just $ s' -- TODO: Winning Condition
        Rock
          | x'==x+1 && y'==y &&
            inRange (bounds m) (x+2,y) &&  m ! (x+2,y) == Empty ->
              -- Additionally, the Rock moves to (x+2,y).
              Just $ s'{ gMap = gMap s' // [((x+2,y), Rock)] }
          | x'==x-1 && y'==y &&
            inRange (bounds m) (x-2,y) &&  m ! (x-2,y) == Empty ->
              -- Additionally, the Rock moves to (x-2,y).
              Just $ s'{ gMap = gMap s' // [((x-2,y), Rock)] }
        _ -> Just s -- invalid
      where
        m' = m // [((x,y), Empty), ((x',y'), Robot)]    
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
