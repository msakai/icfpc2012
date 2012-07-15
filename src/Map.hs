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
  | HigherOrderRock
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
  HigherOrderRock  -> '@'

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

update :: Map -> Bool -> Either Map Map
update m growBeard = if crash m xs then Left (m // xs)
                     else Right (m // xs)
  where
    ((x1,y1),(x2,y2)) = bounds m
    match = all (\(pos,cell) -> getCell m pos == cell)

    -- FIXME: マップに残っているラムダを数えるのではなく、
    -- 最初に存在したラムダを全て回収したかを判定しないといけない
    lambdaRemaining = or [e == Lambda || e == HigherOrderRock | e <- elems m]

    xs = do
      y <- [y1..y2]
      x <- [x1..x2]
      let c = m ! (x,y)
      case c of
        Rock
          | match [((x, y-1),Empty)] ->
              [((x,y), Empty), ((x, y-1), c)]
          | isRock (getCell m (x, y-1)) && match [((x+1, y),Empty), ((x+1, y-1),Empty)] ->
              [((x,y), Empty), ((x+1, y-1), c)]
          | isRock (getCell m (x, y-1)) && match [((x-1, y),Empty), ((x-1, y-1),Empty)] ->
              [((x,y), Empty), ((x-1, y-1), c)]
          | match [((x, y-1),Lambda), ((x+1, y),Empty), ((x+1, y-1),Empty)] ->
              [((x,y), Empty), ((x+1, y-1), c)]
        HigherOrderRock
          | match [((x, y-1),Empty)] ->
              [((x,y), Empty), f (x, y-1)]
          | isRock (getCell m (x, y-1)) && match [((x+1, y),Empty), ((x+1, y-1),Empty)] ->
              [((x,y), Empty), f (x+1, y-1)]
          | isRock (getCell m (x, y-1)) && match [((x-1, y),Empty), ((x-1, y-1),Empty)] ->
              [((x,y), Empty), f (x-1, y-1)]
          | match [((x, y-1),Lambda), ((x+1, y),Empty), ((x+1, y-1),Empty)] ->
              [((x,y), Empty), f (x+1, y-1)]
          where
            f (x',y') = ((x',y'), if getCell m (x',y'-1) /= Empty then Lambda else c)
        ClosedLambdaLift | not lambdaRemaining -> 
          [((x,y), OpenLambdaLift)]
        Beard | growBeard -> do
          dx <- [-1..1]
          dy <- [-1..1]
          let x'=x+dx
              y'=y+dy
          guard $ (x,y) /= (x',y')
          guard $ getCell m (x',y') == Empty
          return ((x',y'), Beard)
        _ -> mzero

getCell :: Map -> Pos -> Cell
getCell m p
  | inRange (bounds m) p = m ! p
  | otherwise            = Wall

isRock :: Cell -> Bool
isRock Rock            = True
isRock HigherOrderRock = True
isRock _               = False

crash :: Map -> [(Pos,Cell)] -> Bool
crash _ []     = False
crash m (((x,y),c) : _)
  | (c == Rock || c == Lambda) && getCell m (x,y-1) == Robot = True
    -- ここのLambdaはHigherOrderRockが変化したLambda
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
parseCell '@'  = HigherOrderRock
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
testUpdate = update (parseMap' m1) False == Right (parseMap' m2) 
  where
    m1 = [ "#* *#"
         , "#* *#"
         , "#####"
         ]
    m2 = [ "#   #"
         , "#***#"
         , "#####"
         ]
