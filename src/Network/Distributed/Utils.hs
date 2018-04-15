{-# LANGUAGE OverloadedStrings #-}

module Network.Distributed.Utils where

import           Control.Monad.IO.Class    (MonadIO, liftIO)
import qualified Data.Configurator         as C
import           Data.Functor              (($>))
import           Data.List                 (intersect)
import           Network.Distributed.Types
import           Prelude                   hiding (log)
import           System.Clock
import           System.Console.ANSI
import           System.Directory          (getCurrentDirectory)
import           System.Exit               (ExitCode (..))
import           System.IO                 (BufferMode (..), hGetContents,
                                            hSetBuffering)
import           System.Process

-- CONFIG ==========================================================================
parseNetConfig :: IO NetworkConfig
parseNetConfig = do
  cfg <- C.load [C.Required "network.config"]
  NetworkConfig <$> C.require cfg "net.host" <*> C.require cfg "net.port" <*>
    C.require cfg "net.path"

-- LOGGING ==========================================================================
log :: MonadIO m => String -> m ()
log = log' [[SetColor Foreground Vivid Black]]

logSucc :: MonadIO m => String -> m ()
logSucc = log' [[SetColor Foreground Vivid Green]]

logWarn :: MonadIO m => String -> m ()
logWarn = log' [[SetColor Foreground Dull Red]]

log' :: MonadIO m => [[SGR]] -> String -> m ()
log' styles msg =
  liftIO $ do
    mapM_ setSGR styles
    putStrLn msg
    setSGR [Reset]

-- List Deps ========================================================================
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

runStackBuildT :: IO ()
runStackBuildT = timeIt runStackBuild

runStackBuild :: IO ()
runStackBuild = do
  log "Build invoked..."
  path <- getCurrentDirectory
  callProcess "stack" ["build", "--stack-root", path ++ "/root"]
  logSucc "Build Succesfully Completed."

-- FIND BEST MATCH ==================================================================
getBestPid :: [(Deps, Node)] -> Deps -> (Maybe Node, Int) -> Maybe Node
getBestPid [] _ best = fst best
getBestPid ((curDeps, curPid):xs) cmpDeps curBest
  | curLen > (snd curBest) = recurse (Just curPid, curLen)
  | otherwise = recurse curBest
  where
    curLen = length (curDeps `intersect` cmpDeps)
    recurse = getBestPid xs cmpDeps

-- Timing
timeIt :: MonadIO m => m a -> m a
timeIt action = do
  start <- liftIO $ getTime Monotonic
  res <- action
  end <- liftIO $ getTime Monotonic
  log $ "Time: " ++ show (sec $ diffTimeSpec start end) ++ " seconds"
  pure res
