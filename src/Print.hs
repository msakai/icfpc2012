module Print where

import Text.Printf
import System.Console.ANSI
import GameState

import Map

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


printStateAnsi :: (Int,Int) -> GameState -> IO ()
printStateAnsi (i,j) s = do
  setCursorPosition i 0
  clearFromCursorToScreenEnd
  setCursorPosition i j
  let { lls = showMap' (gMap s)
      ; u   = gHeight s - gWater s
      ; v   = max 0 u
      ; (a,b) = splitAt v lls
      ; wd  = gWidth s
      }
  printOutOfWater j wd a
  setSGR water
  printUnderWater j wd b
  setSGR []
  printf "Steps: %d; Score: %d; Lambda: %d\n" (gSteps s) (gScore s) (gLambda s)
  printf "Water: %d; Flooding: %d; Waterproof: %d; Underwater: %d\n"
    (gWater s) (gFlooding s) (gWaterproof s) (gUnderwater s)
  printf "Growth: %d; Razors: %d\n"
    (gGrowth s) (gRazors s)
  case gEnd s of
    Nothing -> return ()
    Just w -> printf "End: %s\n" $ show w
  where
    water = [SetColor Background Dull Blue, SetColor Foreground Vivid Yellow]

printOutOfWater, printUnderWater :: Int -> Int -> [String] -> IO ()
printOutOfWater i w = mapM_ (layoutPrint w i)
printUnderWater i w = mapM_ (layoutPrint w i)

layoutPrint :: Int -> Int -> String -> IO ()
layoutPrint w i s = setCursorColumn i >> putStr s' >> cursorDownLine 1
  where s' = take w (s ++ repeat ' ')
