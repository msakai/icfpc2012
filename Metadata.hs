module Metadata where

import Control.Monad
import Data.List

{-
data Metadata
  = Metadata
  { mWater      :: !Int
  , mFlooding   :: !Int
  , mWaterproof :: !Int
  }
-}

parseMetadata :: String -> [(String, Int)]
parseMetadata = parseMetadata' . lines

parseMetadata' :: [String] -> [(String, Int)]
parseMetadata' ls = do
  l <- ls
  key <- keys
  case stripPrefix key l of
    Just (' ':s) -> return (key, read s)
    _ -> mzero

keys :: [String]
keys = ["Water", "Flooding", "Waterproof"]

sampleMetadata =
  parseMetadata "Water 0\nFlooding 10\nWaterproof 5\n"

test_parseMetadata =
  sampleMetadata == [("Water", 0), ("Flooding", 10), ("Waterproof", 5)]
