{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
module Map where

import Control.Monad
import Control.Monad.ST
import Data.Array
import Data.Array.ST hiding (unsafeFreeze)
import Data.Array.Unsafe
import Data.List
import Data.STRef

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

update :: Map -> Bool -> Bool -> Either Map Map
update m lambdaRemaining growBeard = runST $ do
  result <- thaw m
  crashRef <- newSTRef False
  updateST m lambdaRemaining growBeard crashRef result
  crash <- readSTRef crashRef
  m' <- unsafeFreeze result
  return $!
    if crash
    then Left m'
    else Right m'

updateST :: Map -> Bool -> Bool -> STRef s Bool -> STArray s Pos Cell -> ST s ()
updateST m lambdaRemaining growBeard crash result = sequence_ [updatePos x y | x <- [x1..x2], y <- [y1..y2]]
  where
    ((x1,y1),(x2,y2)) = bounds m
    match = all (\(pos,cell) -> getCell m pos == cell)

    updatePos x y = do
      let c = m ! (x,y)
      case c of
        _ | isRock c && match [((x, y-1),Empty)] -> do
              clearXY
              f (x, y-1)
          | isRock c && isRock (getCell m (x, y-1)) && match [((x+1, y),Empty), ((x+1, y-1),Empty)] -> do
              clearXY
              f (x+1, y-1)
          | isRock c && isRock (getCell m (x, y-1)) && match [((x-1, y),Empty), ((x-1, y-1),Empty)] -> do
             clearXY
             f (x-1, y-1)
          | isRock c && match [((x, y-1),Lambda), ((x+1, y),Empty), ((x+1, y-1),Empty)] -> do
              clearXY
              f (x+1, y-1)
          where
            clearXY = writeArray result (x,y) Empty
            f (x',y') = do
              let below = getCell m (x',y'-1)
                  c' = if c == HigherOrderRock && below /= Empty then Lambda else c
              when (below == Robot) $ writeSTRef crash True
              writeArray result (x',y') c'
        ClosedLambdaLift | not lambdaRemaining -> 
          writeArray result (x,y) OpenLambdaLift
        Beard | growBeard -> sequence_ $ do
          dx <- [-1..1]
          dy <- [-1..1]
          let x'=x+dx
              y'=y+dy
          guard $ (x,y) /= (x',y')
          guard $ getCell m (x',y') == Empty
          return $ writeArray result (x',y') Beard
        _ -> return ()

getCell :: Map -> Pos -> Cell
getCell m p
  | inRange (bounds m) p = m ! p
  | otherwise            = Wall

isRock :: Cell -> Bool
isRock Rock            = True
isRock HigherOrderRock = True
isRock _               = False

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
parseCell c    = error ("parseCell: parse error: " ++ show c)

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
testUpdate = update (parseMap' m1) False False == Right (parseMap' m2) 
  where
    m1 = [ "#* *#"
         , "#* *#"
         , "#####"
         ]
    m2 = [ "#   #"
         , "#***#"
         , "#####"
         ]
