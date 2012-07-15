{-# OPTIONS_GHC -Wall #-}
module Move where

import Data.Array
import Map

data Command
  = L
  | R
  | U
  | D
  | W
  | A
  | S
  deriving (Ord, Eq, Show, Read, Bounded, Enum)

parseCommands :: String -> [Command]
parseCommands = map (read . return)

showCommands :: [Command] -> String
showCommands = concat . map show

data EndingCondition
  = Winning
  | Losing
  | Abort
  deriving (Eq, Ord, Enum, Show)

isValidMove :: Map -> Pos -> Pos -> Bool
isValidMove m (x,y) (x',y') =
  case m ! (x',y') of
    Empty          -> True
    Earth          -> True
    Lambda         -> True
    OpenLambdaLift -> True
    Razor          -> True
    c
      | isRock c && x'==x+1 && y'==y &&
        inRange (bounds m) (x+2,y) &&  m ! (x+2,y) == Empty ->
          True
          -- Additionally, the Rock moves to (x+2,y).
      | isRock c && x'==x-1 && y'==y &&
        inRange (bounds m) (x-2,y) &&  m ! (x-2,y) == Empty ->
          True
          -- Additionally, the Rock moves to (x−2,y).
    _ -> False
