module Move where

import Map

data Move
  = L
  | R
  | U
  | D
  | W
  | A
  deriving (Ord, Eq, Show)

move :: Move -> (Map, (Int,Int)) -> Maybe (Map, (Int,Int))
move move (m,(x,y)) =
  case move of
    L -> f (x-1, y)
    R -> f (x+1, y)
    U -> f (x, y+1)
    D -> f (x, y-1)
    W -> Just (m, (x,y))
    A -> Nothing
  where
    f (x',y') = 
      case m ! (x',y') of
        Empty          -> Just (m', (x',y'))
        Earch          -> Just (m', (x',y'))
        Lambda         -> Just (m', (x',y'))
        OpenLambdaLift -> Just (m', (x',y'))
        Rock
          | x'==x+1 && y'==y &&
            inRange (bounds m) (x+2,y) &&  m ! (x+2,y) == Empty ->
              -- Additionally, the Rock moves to (x+2,y).
              Just (m' // [((x+2,y), Rock)],  (x',y'))
          | x'==x-1 && y'==y &&
            inRange (bounds m) (x-2,y) &&  m ! (x-2,y) == Empty ->
              -- Additionally, the Rock moves to (x−2,y).
              Just (m' // [((x-2,y), Rock)],  (x',y'))
        _ -> Just (m, (x,y)) -- invalid
      where
        m' = m // [((x,y),Empty), ((x',y'), Robot)]    

isValidMove :: Map -> (Int,Int) -> (Int,Int) -> Bool
isValidMove m (x,y) (x',y') =
  case m ! (x',y') of
    Empty          -> True
    Earch          -> True
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
