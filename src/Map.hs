{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
module Map where

import Control.Monad
import Data.Array
import Data.List

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
  | Trampoline Char
  | Target Char
  | Beard
  | Razor
  deriving (Ord, Eq, Show)

cell2Char :: Cell -> Char
cell2Char cell = case cell of
  Robot            -> 'R'
  Wall             -> '#'
  Rock             -> '*'
  Lambda           -> '\\'
  ClosedLambdaLift -> 'L'
  OpenLambdaLift   -> 'O'
  Earth            -> '.'
  Empty            -> ' '
  Trampoline c     -> c
  Target c         -> c
  Beard            -> 'W'
  Razor            -> '!'

showMap :: Map -> String
showMap = unlines . showMap'

showMap' :: Map -> [String]
showMap' mp
 = reverse   $ transpose
             $ slices m 
             $ map cell2Char 
             $ elems mp
   where (_,(_,m)) = bounds mp

slices :: Int -> [a] -> [[a]]
slices n = unfoldr phi
  where
    phi [] = Nothing
    phi xs = Just $ splitAt n xs

update :: Map -> Either Map Map
update m = if crash m xs then Left (m // xs)
           else Right (m // xs)
  where
    ((x1,y1),(x2,y2)) = bounds m
    match = all (\(pos,cell) -> getCell m pos == cell)
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

getCell :: Map -> Pos -> Cell
getCell m p
  | inRange (bounds m) p = m ! p
  | otherwise            = Wall

crash :: Map -> [(Pos,Cell)] -> Bool
crash _ []     = False
crash m (((x,y),Rock) : _)
  | getCell m (x,y-1) == Robot = True
crash m (_:cs) = crash m cs

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

parseCell :: Char -> Cell
parseCell 'R'  = Robot
parseCell '#'  = Wall
parseCell '*'  = Rock
parseCell '\\' = Lambda
parseCell 'L'  = ClosedLambdaLift
parseCell 'O'  = OpenLambdaLift
parseCell '.'  = Earth
parseCell ' '  = Empty
parseCell 'W'  = Beard
parseCell '!'  = Razor
parseCell c 
  | c `elem` "ABCDEFGHI" = Trampoline c
  | c `elem` "123456789" = Target c
parseCell _    = error "parseCell: parse error"

showCell :: Cell -> Char
showCell = cell2Char

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
testUpdate = update (parseMap' m1) == Right (parseMap' m2)
  where
    m1 = [ "#* *#"
         , "#* *#"
         , "#####"
         ]
    m2 = [ "#   #"
         , "#***#"
         , "#####"
         ]
