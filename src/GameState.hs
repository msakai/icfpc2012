module GameState where

import Data.Array
import Text.Printf

import Map
import Metadata
import Move (EndingCondition)

data GameState
 = GameState
 { gMap        :: !Map                   -- ^ 地図
 , gHeight     :: !Int                   -- ^ 地図の高さ
 , gWidth      :: !Int                   -- ^ 地図の幅
 , gPos        :: !Pos                   -- ^ ロボットの現在位置
 , gScore      :: !Int                   -- ^ スコア
 , gLambda     :: !Int                   -- ^ 獲得したラムダの数
 , gLambdaLeft :: [Pos]                  -- ^ 未回収のラムダの位置のリスト
 , gSteps      :: !Int                   -- ^ 実行ステップ数
 , gEnd        :: Maybe EndingCondition  -- ^ 終了条件
 , gFallings   :: [Pos]                  -- ^ 落下中の岩の位置
 , gWater      :: !Int                   -- ^ 現在の水位                      
 , gFlooding   :: !Int                   -- ^ 水位上昇ペース                  
 , gWaterproof :: !Int                   -- ^ ロボットが水中にいて大丈夫な時間
 , gUnderwater :: !Int                   -- ^ 現在ロボットが水面下にいる継続時間
 , gTrampoline :: Array Char (Maybe Pos) -- ^ トランポリン（ターゲット位置）
 , gTarget     :: Array Char [Pos]       -- ^ ターゲット（トランポリン位置）
 , gGrowth     :: !Int                   -- ^ 髭の成長率
 , gBreard     :: [Pos]                  -- ^ 髭の位置
 , gRazors     :: !Int                   -- ^ ロボットが持つ剃刀の数
 }
 deriving (Eq, Show)

initialState :: Map -> Metadata -> GameState
initialState m meta
  = GameState
  { gMap    = m
  , gHeight = h
  , gWidth  = w
  , gPos    = head [ i | (i,Robot) <- assocs m]
  , gScore  = 0
  , gLambda = 0
  , gLambdaLeft = [ i | (i,Lambda) <- assocs m]
  , gSteps      = 0
  , gEnd        = Nothing
  , gFallings   = undefined -- [ i | (i,Rock) <- assocs m, falling i]
  , gWater      = fWater finfo
  , gFlooding   = fFlooding finfo
  , gWaterproof = fWaterproof finfo
  , gUnderwater = 0
  , gTrampoline = undefined
  , gTarget     = undefined
  , gGrowth     = grGrowth ginfo
  , gBreard     = undefined
  , gRazors     = grRazors ginfo
  }
  where
    finfo = metaFloodingInfo meta
    ginfo = metaGrowthInfo meta
    (_,(w,h)) = bounds m

initialStateFromString :: String -> GameState
initialStateFromString s = initialState m meta
  where
    ls = lines s
    (ls1,ls2) = break ([]==) ls
    m    = parseMap' ls1
    meta = parseMetadata' (dropWhile null ls2)

printState :: GameState -> IO ()
printState s = do
  putStr $ showMap (gMap s)
  printf "Steps: %d; Score: %d; Lambda: %d\n" (gSteps s) (gScore s) (gLambda s)
  printf "Water: %d; Flooding: %d; Waterproof: %d; Underwater: %d\n"
    (gWater s) (gFlooding s) (gWaterproof s) (gUnderwater s)
  case gEnd s of
    Nothing -> return ()
    Just w -> printf "End: %s\n" $ show w

