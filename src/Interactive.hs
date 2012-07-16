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

printSim :: GameState -> [Command] -> IO ()
printSim s ms = do
  forM_ (simulate s ms) $ \s' -> do
    printState s'
    putStrLn ""

interactiveSim :: Bool -> GameState -> IO ()
interactiveSim vi s0 = go (s0,Seq.empty) []
  where
    getLine' = if vi then getLineVi else getLine
    go curr@(s,_) undoBuf = do
      printStateAnsi (2,2) s
      putStrLn ""
      prompt curr undoBuf
    prompt curr@(s,trace) undoBuf = do
      putStr "> "
      hFlush stdout
      l <- liftM (filter (not . isSpace)) getLine'
      case l of
        ":q"    -> return ()
        ":quit" -> return ()
        ":dump" -> do
          putStrLn $ showCommands (F.toList trace)
          prompt (s,trace) undoBuf
        ":undo" -> do
          case undoBuf of
            [] -> do
              putStrLn "empty undo buffer"
              go (s,trace) undoBuf
            (old:undoBuf') -> go old undoBuf'
        ":reset" -> do
          case undoBuf of
            [] -> go (s,trace) undoBuf
            _ -> go (last undoBuf) []
        _ | isNothing (gEnd s) && all (`elem` "LRUDWAS") (map toUpper l) -> do
          let cs = parseCommands (map toUpper l)
          go (stepN s cs, trace <> Seq.fromList cs) (curr : undoBuf)
        _ -> do
          putStrLn "parse error"
          prompt curr undoBuf

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
