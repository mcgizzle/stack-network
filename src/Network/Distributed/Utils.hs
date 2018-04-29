{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module:      Network.Distributed.Transfer
-- Copyright:   (c) 2018 Sean McGroarty
-- License:     BSD3
-- Maintainer:  Sean McGroarty <mcgroas@tcd.ie.com>
-- Stability:   experimental
--
module Network.Distributed.Utils
  ( parseNetConfig
  , log
  , logSucc
  , logWarn
  , listDeps
  , getBestPid
  , timeIt
  , runStackBuild
  , runStackBuildT
  ) where

-------------------------------------------------------------------------------------
import           Network.Distributed.Types

-------------------------------------------------------------------------------------
import           Control.Monad.IO.Class    (MonadIO, liftIO)
import qualified Data.Configurator         as C
import           Data.List                 (intersect)
import           Prelude                   hiding (log)
import           System.Clock
import           System.Console.ANSI
import           System.Directory          (getCurrentDirectory)
import           System.Exit               (ExitCode (..))
import           System.IO                 (BufferMode (..), hGetContents,
                                            hSetBuffering)
import           System.Process

-------------------------------------------------------------------------------------
-- | Parsers configuration from provided @network.config@ file
parseNetConfig :: IO NetworkConfig
parseNetConfig = do
  cfg <- C.load [C.Required "network.config"]
  NetworkConfig <$> C.require cfg "net.host" <*> C.require cfg "net.port"

-------------------------------------------------------------------------------------
-- | Logs to stdout in grey
log :: MonadIO m => String -> m ()
log = log' [[SetColor Foreground Vivid Black]]

-- | Logs to stdout in green
logSucc :: MonadIO m => String -> m ()
logSucc = log' [[SetColor Foreground Vivid Green]]

-- | Logs to stdout in red
logWarn :: MonadIO m => String -> m ()
logWarn = log' [[SetColor Foreground Dull Red]]

-- | Internal log function
log' :: MonadIO m => [[SGR]] -> String -> m ()
log' styles msg =
  liftIO $ do
    mapM_ setSGR styles
    putStrLn msg
    setSGR [Reset]

---------------------------------------------------------------------------------------
-- | Determines a nodes dependencies
listDeps :: MonadIO m => m [String]
listDeps =
  liftIO $ do
    path <- getCurrentDirectory
    (_, Just hStdout, _, p) <-
      System.Process.createProcess
        (proc "stack" ["list-dependencies", "--stack-root", path ++ "/root"])
        {std_out = CreatePipe, std_err = Inherit}
    hSetBuffering hStdout NoBuffering
    exit_code <- waitForProcess p
    case exit_code of
      ExitSuccess   -> lines <$> hGetContents hStdout
      ExitFailure _ -> logWarn "Error calculating dependencies" >> pure []

-- | Finds the ProcessId with the most overlapp, returning Nothing if there is no overlapp
getBestPid ::
     [(Deps, Node)]
  -- ^ a list of pairs of Nodes and their dependencies
  -> Deps
  -- ^ Master nodes dependencies
  -> (Maybe Node, Int)
  -- ^ Current best. Initially set to (Nothing,0)
  -> Maybe Node
getBestPid [] _ best = fst best
getBestPid ((curDeps, curPid):xs) cmpDeps curBest
  | curLen > snd curBest = recurse (Just curPid, curLen)
  | otherwise = recurse curBest
  where
    curLen = length (curDeps `intersect` cmpDeps)
    recurse = getBestPid xs cmpDeps

-------------------------------------------------------------------------------------
runStackBuildT :: IO ()
runStackBuildT = timeIt runStackBuild

runStackBuild :: IO ()
runStackBuild = do
  log "Build invoked..."
  path <- getCurrentDirectory
  callProcess "stack" ["build", "--stack-root", path ++ "/root"]
  logSucc "Build Succesfully Completed."

-------------------------------------------------------------------------------------
-- | Times an action
--
--Logs out the amount of seconds the action took
timeIt ::
     MonadIO m
  => m a
  -- ^ Action to time
  -> m a
timeIt action = do
  start <- liftIO $ getTime Monotonic
  res <- action
  end <- liftIO $ getTime Monotonic
  logSucc $ "Time: " ++ show (sec $ diffTimeSpec start end) ++ " seconds"
  pure res
