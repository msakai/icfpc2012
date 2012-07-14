{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
module Map where

import Control.Monad
import Data.Array

type Map = Array Pos Cell

type Pos = (Int,Int)

data Cell
  = Robot
  | Wall
  | Rock
  | Lambda
  | ClosedLambdaLift
  | OpenLambdaLift
  | Earth
  | Empty
  deriving (Ord, Eq, Show)

update :: Map -> Maybe Map
update m = Just (m // xs)
  where
    ((x1,y1),(x2,y2)) = bounds m

    match = all (\(pos,cell) -> get pos == cell)

    get p
      | inRange (bounds m) p = m ! p
      | otherwise = Wall

    lambdaRemaining = Lambda `elem` elems m

    xs = do
      y <- [y1..y2]
      x <- [x1..x2]
      let c = m ! (x,y)
      case c of
        Rock
          | match [((x, y-1),Empty)] ->
              [((x,y), Empty), ((x, y-1), Rock)]
          | match [((x, y-1),Rock), ((x+1, y),Empty), ((x+1, y-1),Empty)] ->
              [((x,y), Empty), ((x+1, y-1), Rock)]
          | match [((x, y-1),Rock), ((x-1, y),Empty), ((x-1, y-1),Empty)] ->
              [((x,y), Empty), ((x-1, y-1), Rock)]
          | match [((x, y-1),Lambda), ((x+1, y),Empty), ((x+1, y-1),Empty)] ->
              [((x,y), Empty), ((x+1, y-1), Rock)]
        ClosedLambdaLift | not lambdaRemaining -> 
          [((x,y), OpenLambdaLift)]
        _ -> mzero

parseMap :: String -> Map
parseMap = parseMap' . lines

parseMap' :: [String] -> Map
parseMap' ls = array ((1,1),(n,m)) $ do
  (y,l) <- zip [m,m-1..] ls
  (x,c) <- zip [1..n] (l ++ repeat ' ')
  return ((x,y), parseCell c)
  where
    m = length ls
    n = maximum [length l | l<-ls]

showMap :: Map -> String
showMap = unlines . showMap'

showMap' :: Map -> [String]
showMap' m = [[showCell (m ! (x,y)) | x <- [x1..x2]] | y <- [y2,y2-1..y1]]
  where
    ((x1,y1),(x2,y2)) = bounds m

parseCell :: Char -> Cell
parseCell 'R'  = Robot
parseCell '#'  = Wall
parseCell '*'  = Rock
parseCell '\\' = Lambda
parseCell 'L'  = ClosedLambdaLift
parseCell 'O'  = OpenLambdaLift
parseCell '.'  = Earth
parseCell ' '  = Empty
parseCell _    = error "parseCell: parse error"

showCell :: Cell -> Char
showCell Robot            = 'R'
showCell Wall             = '#'
showCell Rock             = '*'
showCell Lambda           = '\\'
showCell ClosedLambdaLift = 'L'
showCell OpenLambdaLift   = 'O'
showCell Earth            = '.'
showCell Empty            = ' '

testParseMap :: Map
testParseMap = parseMap' m
  where
    m = [ "#* *#"
        , "#* *#"
        , "#####"
        ]

testShowMap :: Bool
testShowMap = showMap' (parseMap' m) == m
  where
    m = [ "#* *#"
        , "#* *#"
        , "#####"
        ]

testUpdate :: Bool
testUpdate = update (parseMap' m1) == Just (parseMap' m2)
  where
    m1 = [ "#* *#"
         , "#* *#"
         , "#####"
         ]
    m2 = [ "#   #"
         , "#***#"
         , "#####"
         ]

contest1 :: Map
contest1 =  parseMap'
  [ "######"
  , "#. *R#"
  , "#  \\.#"
  , "#\\ * #"
  , "L  .\\#"
  , "######"
  ]

contest2 :: Map
contest2 = parseMap'
  [ "#######"
  , "#..***#"
  , "#..\\\\\\#"
  , "#...**#"
  , "#.*.*\\#"
  , "LR....#"
  , "#######"
  ]
