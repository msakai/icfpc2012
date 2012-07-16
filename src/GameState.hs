module GameState where

import Data.Array
import Data.Maybe
import Data.List
import Data.Ord
import Text.Printf

import Map
import Metadata
import Move (EndingCondition)

import Debug.Trace

data GameState
 = GameState
 { gMap        :: !Map                   -- ^ 地図
 , gHeight     :: !Int                   -- ^ 地図の高さ
 , gWidth      :: !Int                   -- ^ 地図の幅
 , gPos        :: !Pos                   -- ^ ロボットの現在位置
 , gScore      :: !Int                   -- ^ スコア
 , gLambda     :: !Int                   -- ^ 獲得したラムダの数
 , gReqLambda  :: !Int                   -- ^ クリアに必要なラムダの数
 , gLambdaLeft :: [Pos]                  -- ^ 未回収のラムダの位置のリスト
 , gSteps      :: !Int                   -- ^ 実行ステップ数
 , gEnd        :: Maybe EndingCondition  -- ^ 終了条件
 , gFallings   :: [(Pos,Pos)]            -- ^ 落下しようとする岩の位置と落下方向
 , gWater      :: !Int                   -- ^ 現在の水位                      
 , gFlooding   :: !Int                   -- ^ 水位上昇ペース                  
 , gWaterproof :: !Int                   -- ^ ロボットが水中にいて大丈夫な時間
 , gUnderwater :: !Int                   -- ^ 現在ロボットが水面下にいる継続時間
 , gTrampoline :: [(Char, Pos)]          -- ^ トランポリン（ターゲット位置）
 , gTarget     :: [(Char, [Pos])]        -- ^ ターゲット（トランポリン位置）
 , gGrowth     :: !Int                   -- ^ 髭の成長率
 , gBreard     :: [Pos]                  -- ^ 髭の位置
 , gRazors     :: !Int                   -- ^ ロボットが持つ剃刀の数
 }
 deriving (Eq, Show)

printState :: GameState -> IO ()
printState s = do
  putStr $ showMap (gMap s)
  printf "Steps: %d; Score: %d; Lambda: %d\n" (gSteps s) (gScore s) (gLambda s)
  printf "Water: %d; Flooding: %d; Waterproof: %d; Underwater: %d\n"
    (gWater s) (gFlooding s) (gWaterproof s) (gUnderwater s)
  printf "Growth: %d; Razors: %d\n"
    (gGrowth s) (gRazors s)
  case gEnd s of
    Nothing -> return ()
    Just w -> printf "End: %s\n" $ show w

initialState :: Map -> Metadata -> GameState
initialState m meta
  = GameState
  { gMap    = m
  , gHeight = h
  , gWidth  = w
  , gPos    = head [ i | (i,Robot) <- assocs m]
  , gScore  = 0
  , gLambda = 0
  , gReqLambda  = length [() | Lambda <- elems m] + length [() | HigherOrderRock <- elems m]
  , gLambdaLeft = [ i | (i,Lambda) <- assocs m]
  , gSteps      = 0
  , gEnd        = Nothing
  , gFallings   = [ (i,fromJust fallto) 
                  | (i,c) <- assocs m
                  , isRock c
                  , let fallto = falling m i
                  , fallto /= Nothing]
  , gWater      = fWater finfo
  , gFlooding   = fFlooding finfo
  , gWaterproof = fWaterproof finfo
  , gUnderwater = 0
  , gTrampoline = map (\(f,t) -> (f,fromJust $ lookup t topos)) ftassc
  , gTarget     = map gather 
                $ groupBy (\ x y -> fst x == fst y)
                $ sort [ (t,f) | (f,t) <- ftassc ]
  , gGrowth     = grGrowth ginfo
  , gBreard     = [ i | (i,Beard) <- assocs m ]
  , gRazors     = grRazors ginfo
  }
  where
    (_,(w,h)) = bounds m
    finfo = metaFloodingInfo meta
    ginfo = metaGrowthInfo meta
    tinfo = metaTrampolineInfo meta
    ftassc= tTrampoline tinfo
    topos = [ (c,pos) | (pos,Target c) <- assocs m ]
    frpos = [ (c,pos) | (pos,Trampoline c) <- assocs m ]
    gather a@((t,_):_) = (t,concatMap (\ (_,s) -> lookfor s frpos) a)

initialStateFromString :: String -> GameState
initialStateFromString s = initialState m meta
  where
    ls = lines s
    (ls1,ls2) = break ([]==) ls
    m    = parseMap' ls1
    meta = parseMetadata' (dropWhile null ls2)

falling :: Map -> Pos -> Maybe Pos
falling m p 
  = if south == Empty 
    then Just sp
    else if isRock south
         then if east == Empty && southeast == Empty
              then Just sep
              else if west == Empty && southwest == Empty
                   then Just swp
                   else Nothing
         else if south == Lambda && east == Empty && southeast == Empty
              then Just sep
              else Nothing
 where
   s  (x,y) = (x,y-1)
   e  (x,y) = (x+1,y)
   se (x,y) = (x+1,y-1)
   w  (x,y) = (x-1,y)
   sw (x,y) = (x-1,y-1)
   sp  = s p
   ep  = e p
   sep = se p
   wp  = w p
   swp = sw p
   south = getCell m sp
   east  = getCell m ep
   west  = getCell m wp
   southeast = getCell m sep
   southwest = getCell m swp
