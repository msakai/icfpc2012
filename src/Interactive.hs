module Interactive where

import Control.Monad
import Data.Char
import Data.Maybe
import Data.Monoid
import qualified Data.Foldable as F
import qualified Data.Sequence as Seq
import System.IO

import Move
import Sim
import Print

data Options
  = Options
  { optVi   :: Bool
  , optAnsi :: Bool
  }

printSim :: GameState -> [Command] -> IO ()
printSim s ms = do
  forM_ (simulate s ms) $ \s' -> do
    printState s'
    putStrLn ""

interactiveSim :: Options -> GameState -> IO ()
interactiveSim opt s0 = go s0 []
  where
    getLine' = if optVi opt then getLineVi else getLine
    go s undoBuf = do
      if optAnsi opt
        then printStateAnsi (2,2) s
        else printState s
      putStrLn ""
      prompt s undoBuf
    prompt s undoBuf = do
      putStr "> "
      hFlush stdout
      l <- liftM (filter (not . isSpace)) getLine'
      case l of
        ":q"    -> return ()
        ":quit" -> return ()
        ":dump" -> do
          putStrLn $ showCommands $ reverse $ gHistory s
          prompt s undoBuf
        ":undo" -> do
          case undoBuf of
            [] -> do
              putStrLn "empty undo buffer"
              go s undoBuf
            (old:undoBuf') -> go old undoBuf'
        ":reset" -> do
          case undoBuf of
            [] -> go s undoBuf
            _ -> go (last undoBuf) []
        _ | isNothing (gEnd s) && all (`elem` "LRUDWAS") (map toUpper l) -> do
          let cs = parseCommands (map toUpper l)
          go (stepN s cs) (s : undoBuf)
        _ -> do
          putStrLn "parse error"
          prompt s undoBuf

getLineVi :: IO String
getLineVi = do { oldEcho      <- hGetEcho stdin
               ; oldBuffering <- hGetBuffering stdin
               ; hSetBuffering stdin NoBuffering
               ; hSetEcho stdin False
               ; s <- loop
               ; hSetEcho stdin oldEcho
               ; hSetBuffering stdin oldBuffering
               ; return s
               }

loop :: IO String
loop = do { c  <- getChar
          ; c' <- case c of
                   'h' -> echo 'L'
                   'j' -> echo 'D'
                   'k' -> echo 'U'
                   'l' -> echo 'R'
                   'a' -> echo 'A'
                   's' -> echo 'S'
                   _   -> echo c
          ; if '\n'== c then return ""
            else loop >>= return . (c':)
          }

echo :: Char -> IO Char
echo c = putChar c >> hFlush stdout >> return c
