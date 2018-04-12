{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Distributed.Internal where

import           Network.Distributed.Types
import           Network.Distributed.Utils

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Catch                                (bracket)
import           Prelude                                            hiding
                                                                     (FilePath,
                                                                     log)

import           Control.Distributed.Process                        hiding
                                                                     (bracket)
import           Control.Distributed.Process.Backend.SimpleLocalnet
import qualified Control.Distributed.Process.Node                   as PN

import           Data.DirStream
import           Data.Maybe                                         (catMaybes)

import           Filesystem
import           Filesystem.Path                                    (FilePath,
                                                                     directory)
import           Filesystem.Path.CurrentOS                          (decode,
                                                                     encode)

import           Pipes
import qualified Pipes.Prelude                                      as P
import           Pipes.Safe                                         (MonadMask,
                                                                     MonadSafe,
                                                                     runSafeT)

-- DISTRIBUTED NODES ============================================================
runRequestNode :: NetworkConfig -> IO ()
runRequestNode NetworkConfig {..} = do
  backend <-
    initializeBackend hostNetworkConfig portNetworkConfig PN.initRemoteTable
  node <- newLocalNode backend
  PN.runProcess node (runRequestNode' backend)
  runStackBuildT

runRequestNode' :: Backend -> Process ()
runRequestNode' backend = do
  me <- getSelfPid
  log "Searching the Network..."
  pids <- findPids backend
  logSucc $ "Found Nodes: " ++ show pids
  pDeps <- gatherDeps me pids
  myDeps <- listDeps
  log "Finding most compatable node..."
  case getBestPid pDeps myDeps (Nothing, 0) of
    Nothing -> do
      logWarn "No Nodes share dependencies, aborting."
    Just n -> do
      log $ "Node: " ++ show n ++ " Is the best match."
      (sPort, rPort) <- newChan
      send n (TransferReq sPort)
      log "Working..."
      receiveF rPort
      logSucc "Transmission complete. All files received. Ready to build."
      mapM_ (flip send Terminate) pids

joinNetwork :: NetworkConfig -> IO ()
joinNetwork nc@NetworkConfig {..} = do
  log $ "Node joining network on: " ++ show nc
  node <-
    newLocalNode =<<
    initializeBackend hostNetworkConfig portNetworkConfig PN.initRemoteTable
  PN.runProcess node joinNetwork'

joinNetwork' :: Process ()
joinNetwork' = do
  me <- getSelfPid
  register "nodeS" me
  loop me
  where
    loop me = do
      receiveWait [match $ \req -> receiveReq me req]
      loop me
    receiveReq :: ProcessId -> Request -> Process ()
    receiveReq me (Ping pid) = do
      log "Received a Ping!"
      deps <- listDeps
      send pid (PD (deps, me))
      loop me
    receiveReq me (TransferReq sPort) = do
      log "Received request to transmit files... Beginning transmission"
      pipeFiles (sendChan sPort) *> sendChan sPort TransferDone
      logSucc "Transmission complete"
      loop me
    receiveReq _ Terminate = log "Received a request to terminate\nBye."

-- HELPER FUNCTIONS ===============================================================
findPids :: Backend -> Process [ProcessId]
findPids backend = do
  loop
  where
    loop = do
      nids <- liftIO $ findPeers backend 1000000
      pids <-
        bracket (mapM monitorNode nids) (mapM unmonitor) $ \_ -> do
          forM_ nids $ \n -> whereisRemoteAsync n "nodeS"
          catMaybes <$>
            replicateM
              (length nids)
              (receiveWait [match (\(WhereIsReply "nodeS" mPid) -> pure mPid)])
      if null pids
        then loop
        else pure pids

gatherDeps :: ProcessId -> Network -> Process [ProcessDeps]
gatherDeps me pids = do
  mapM_ (flip send (Ping me)) pids
  replicateM (length pids) (receiveWait [match $ \(PD pd) -> pure pd])

-----------------------------------------------------------------------------------
-- RECEIVE FILES ==================================================================
receiveF :: ReceivePort Transfer -> Process ()
receiveF rPort = work =<< receiveChan rPort
  where
    work :: Transfer -> Process ()
    work (TransferInProg i) = do
      liftIO $ saveFile i
      receiveF rPort
    work TransferDone = pure ()
    saveFile :: FileInfo -> IO ()
    saveFile (path, file) = do
      createTree (directory path')
      Filesystem.writeFile path' file
      where
        path' = decode path

-- SEND FILES =====================================================================
pipeFiles :: (MonadMask m, MonadIO m) => (Transfer -> m ()) -> m ()
pipeFiles action = do
  runSafeT $
    runEffect $
    for
      (producers >-> P.filterM (liftIO . isFile) >-> P.chain (log . show) >->
       P.mapM packageFile)
      (lift . lift . action)

producers :: MonadSafe m => Pipes.Proxy x' x () FilePath m ()
producers =
  every
    (descendentOf "root/snapshots" <|> descendentOf "root/loaded-snapshot-cache")

packageFile :: MonadIO m => FilePath -> m Transfer
packageFile file =
  liftIO $ do
    conts <- Filesystem.readFile file
    pure $ TransferInProg (encode file, conts)
-----------------------------------------------------------------------------------
