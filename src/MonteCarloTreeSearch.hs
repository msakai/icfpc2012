module MonteCarloTreeSearch
  ( run
  ) where

import Control.Monad
import Data.IORef
import Data.List
import Data.Maybe
import Data.Ord
import qualified System.Random as Rand
import System.IO

import Move
import GameState
import Sim

data Tree
  = Node
  { ndState     :: GameState
  , ndCommands  :: [Command]
  , ndChildren  :: IORef (Maybe [(Command, Tree)]) -- 展開前はNothing
  , ndBestScore :: IORef Int            -- その先に進んでこれまで見つかった最善の値
  , ndTrial     :: IORef Int            -- このノードに到達した回数
  }

newNode :: GameState -> [Command] -> IO Tree
newNode s cmds = do
  children <- newIORef Nothing
  best     <- newIORef (gScore s)
  trial    <- newIORef 0
  return $
    Node
    { ndState     = s
    , ndCommands  = cmds
    , ndChildren  = children
    , ndBestScore = best
    , ndTrial     = trial
    }

expand :: Tree -> IO [(Command, Tree)]
expand n = do
  r <- readIORef (ndChildren n)
  case r of
    Just xs -> return xs
    Nothing -> do
      children <- sequence
        [ do n' <- newNode (step (ndState n) c) (c : ndCommands n)
             return (c, n')
        | isNothing (gEnd (ndState n))
        , c <- [minBound..maxBound]
        , isMeaningfulCommand (ndState n) c
        ]
      writeIORef (ndChildren n) (Just children)
      return children  

run :: (GameState -> [Command] -> IO ()) -> GameState -> IO ()
run check s0 = do
  root <- newNode s0 []
  go root root

  where
    check' :: Tree -> [Tree] -> IO ()
    check' nd ancestors = do
      check (ndState nd) (ndCommands nd)
      s <- readIORef (ndBestScore nd)
      updateBest s ancestors

    updateBest :: Int -> [Tree] -> IO ()
    updateBest _ [] = return ()
    updateBest newBest (nd:nds) = do
      oldBest <- readIORef (ndBestScore nd)
      if (oldBest > newBest)
        then return ()
        else do
          writeIORef (ndBestScore nd) newBest
          updateBest newBest nds

    walk :: Tree -> [Tree] -> IO ()
    walk nd ancestors = do
      t <- readIORef (ndTrial nd)
      writeIORef (ndTrial nd) $! (t + 1)

      cs <- expand nd

      -- 初回のみ
      when (t == 0) $ do
        case [nd2 | (A,nd2) <- cs] of
          nd2:_ -> check' nd2 (nd : ancestors)
          _     -> check' nd ancestors
 
      let len = length cs
      when (len > 0) $ do
        i <- Rand.getStdRandom $ Rand.randomR (0, len - 1)
        walk (snd (cs !! i)) (nd : ancestors)

    go root nd = do
      t <- readIORef (ndTrial nd)
      if t < 2000
        then walk nd [] >> go root nd
        else do
          cs <- expand nd
          if null cs
            then do
              go root root
            else do
              xs <- forM cs $ \(_, nd2) -> do
                best <- readIORef (ndBestScore nd2)
                return (nd2,best)
              let nd2 = fst $ maximumBy (comparing snd) xs
              go root nd2
