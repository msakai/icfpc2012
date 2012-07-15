module Metadata where

import Data.Char

data Metadata
 = Metadata
 { metaFloodingInfo    :: FloodingInfo
 , metaTrampolineInfo  :: TrampolineInfo
 , metaGrowthInfo      :: GrowthInfo
 } deriving (Show)

defaultMetadata :: Metadata
defaultMetadata =  Metadata { metaFloodingInfo = defaultFloodingInfo
                            , metaTrampolineInfo = defaultTrampolineInfo
                            , metaGrowthInfo = defaultGrowthInfo
                            }

data FloodingInfo
 = FloodingInfo
 { fWater      :: !Int -- ^ 最初の水位                      
 , fFlooding   :: !Int -- ^ 水位上昇ペース                  
 , fWaterproof :: !Int -- ^ ロボットが水中にいて大丈夫な時間
 } deriving (Show)


defaultFloodingInfo :: FloodingInfo
defaultFloodingInfo = FloodingInfo { fWater      = 0
                                   , fFlooding   = 0
                                   , fWaterproof = 10
                                   }

data TrampolineInfo
 = TrampolineInfo
 { tTrampoline :: [(Char,Char)] -- ^ トランポリン -> ターゲット
 }
   deriving (Show)

defaultTrampolineInfo :: TrampolineInfo
defaultTrampolineInfo = TrampolineInfo { tTrampoline = [] }

data GrowthInfo
 = GrowthInfo
 { grGrowth :: !Int   -- ^ 髭の成長率
 , grRazors :: !Int   -- ^ ロボットが最初に持っている剃刀の数
 } deriving Show

defaultGrowthInfo :: GrowthInfo
defaultGrowthInfo =  GrowthInfo { grGrowth = 25
                                , grRazors = 0
                                }

parseMetadata :: String -> Metadata
parseMetadata = parseMetadata' . lines

parseMetadata' :: [String] -> Metadata
parseMetadata' [] = defaultMetadata
parseMetadata' ls = collect 
                  . map (mkassoc . words . downcase) 
                  . filter (not . null) $ ls
  where
    downcase = map toLower

mkassoc :: [String] -> (String,String)
mkassoc [k@"water",v]        = (k,v)
mkassoc [k@"flooding",v]     = (k,v)
mkassoc [k@"waterproof",v]   = (k,v)
mkassoc ["trampoline",k,_,v] = (k,v)
mkassoc [k@"growth",v]       = (k,v)
mkassoc [k@"razors",v]       = (k,v)
mkassoc s                    = error ("Unknow format: ("++ unwords s++")")

collect :: [(String,String)] -> Metadata
collect s = Metadata
 { metaFloodingInfo    = finfo
 , metaTrampolineInfo  = tinfo
 , metaGrowthInfo      = ginfo
 }
   where
     finfo = FloodingInfo { fWater = w
                          , fFlooding = f
                          , fWaterproof = p
                          }
     tinfo = TrampolineInfo { tTrampoline = t }
     ginfo = GrowthInfo { grGrowth = g
                        , grRazors = r
                        }
     w = parseWater (lookfor "water" s)
     f = parseFlooding (lookfor "flooding" s)
     p = parseWarterProof (lookfor "waterproof" s)
     t = parseTrampoline (filter ((1==).length.fst) s)
     g = parseGrowth (lookfor "growth" s)
     r = parseRazors (lookfor "razors" s)

lookfor :: Eq a => a -> [(a,b)] -> [b]
lookfor _ [] = []
lookfor a ((k,v):rs)
  | a == k    = v : lookfor a rs
  | otherwise = lookfor a rs

parseWater :: [String] -> Int
parseWater []    = fWater defaultFloodingInfo
parseWater (x:_) = read x

parseFlooding :: [String] -> Int
parseFlooding []    = fFlooding defaultFloodingInfo
parseFlooding (x:_) = read x

parseWarterProof :: [String] -> Int
parseWarterProof []    = fWaterproof defaultFloodingInfo
parseWarterProof (x:_) = read x

parseTrampoline :: [(String,String)] -> [(Char, Char)]
parseTrampoline [] = tTrampoline defaultTrampolineInfo
parseTrampoline xs = map (cross (f,g)) xs
  where
    f = toUpper . head
    g = head

cross :: (a->c,b->d) -> (a,b) -> (c,d)
cross (f,g) (x,y) = (f x,g y)

parseGrowth :: [String] -> Int
parseGrowth [] = grGrowth defaultGrowthInfo 
parseGrowth (x:_) = read x

parseRazors :: [String] -> Int
parseRazors [] = grRazors defaultGrowthInfo
parseRazors (x:_) = read x
