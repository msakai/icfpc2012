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
  , ndChildren  :: IORef (Maybe [(Command, Tree)]) -- 展開前はNothing
  , ndBestScore :: IORef Int            -- その先に進んでこれまで見つかった最善の値
  , ndTrial     :: IORef Int            -- このノードに到達した回数
  , ndChecked   :: IORef Bool           -- チェック済みかどうか
  }

ndCommands :: Tree -> [Command]
ndCommands = gHistory . ndState

newNode :: GameState -> IO Tree
newNode s = do
  children <- newIORef Nothing
  best     <- newIORef (gScore s)
  trial    <- newIORef 0
  checked  <- newIORef False
  return $
    Node
    { ndState     = s
    , ndChildren  = children
    , ndBestScore = best
    , ndTrial     = trial
    , ndChecked   = checked
    }

expand :: Tree -> IO [(Command, Tree)]
expand n = do
  r <- readIORef (ndChildren n)
  case r of
    Just xs -> return xs
    Nothing -> do
      children <- sequence
        [ do n' <- newNode (step (ndState n) c)
             return (c, n')
        | isNothing (gEnd (ndState n))
        , c <- [minBound..maxBound]
        , isMeaningfulCommand (ndState n) c
        ]
      writeIORef (ndChildren n) (Just children)
      return children  

run :: (GameState -> IO ()) -> GameState -> IO ()
run check s0 = do
  forM_ (repeat 2000) $ \lim -> do
    let loop nd = do
          t <- readIORef (ndTrial nd)
          if t < lim
            then walk nd [] >> loop nd
            else do
              cs <- expand nd
              if null cs
                then return ()
                else do
                  xs <- forM cs $ \(_, nd2) -> do
                    best <- readIORef (ndBestScore nd2)
                    return (nd2,best)
                  let nd2 = fst $ maximumBy (comparing snd) xs
                  loop nd2
    root <- newNode s0
    loop root

  where
    check' :: Tree -> [Tree] -> IO ()
    check' nd ancestors = do
      checked <- readIORef (ndChecked nd)
      unless checked $ do
        let s  = ndState nd
            s' = step s A
        check s'
        updateBest (gScore s') ancestors
        writeIORef (ndChecked nd) True

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
        check' nd ancestors
        forM_ cs $ \(_, ch) -> do
          check' ch (nd : ancestors)
 
      if gEnd (ndState nd) == Just Winning
        then return ()
        else do
          let cs2 = [nd2 | (_, nd2) <- cs, isNothing $ gEnd (ndState nd2)]
              len = length cs2
          when (len > 0) $ do
            i <- Rand.getStdRandom $ Rand.randomR (0, len - 1)
            walk (cs2 !! i) (nd : ancestors)

gc :: Tree -> IO ()
gc root = do
  best <- readIORef (ndBestScore root)
  let bnd = ceiling $ fromIntegral best * 0.9

  let go :: [Tree] -> IO ()
      go [] = return ()
      go (nd:nds) = do
        s <- readIORef (ndBestScore nd)
        if (bnd > s)
          then do
            writeIORef (ndChildren nd) Nothing
            go nds
          else do
            children <- readIORef (ndChildren nd)
            case children of
              Nothing -> go nds
              Just cs -> go (map snd cs ++ nds)
  go [root]
