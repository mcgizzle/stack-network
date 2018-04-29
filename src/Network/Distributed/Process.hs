{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

-- |
-- Module:      Network.Distributed.Process
-- Copyright:   (c) 2018 Sean McGroarty
-- License:     BSD3
-- Maintainer:  Sean McGroarty <mcgroas@tcd.ie.com>
-- Stability:   experimental
--
module Network.Distributed.Process
  ( runRequestNode
  , runRequestNode'
  , joinNetwork
  , joinNetwork'
  , findPids
  , gatherDeps
  , receiveF
  , withTransfer
  ) where

----------------------------------------------------------------------------------------
import           Network.Distributed.Transfer
import           Network.Distributed.Types
import           Network.Distributed.Utils

----------------------------------------------------------------------------------------
import           Control.Distributed.Process.Backend.SimpleLocalnet (Backend,
                                                                     findPeers,
                                                                     initializeBackend,
                                                                     newLocalNode)
import           Control.Distributed.Process.Lifted                 hiding
                                                                     (bracket,
                                                                     catch)
import           Control.Distributed.Process.Lifted.Class           (MonadProcess)
import qualified Control.Distributed.Process.Node.Lifted            as PN
import           Control.Monad                                      (replicateM,
                                                                     (>=>))
import           Control.Monad.Catch                                (MonadCatch,
                                                                     MonadMask,
                                                                     SomeException,
                                                                     bracket,
                                                                     catch)
import           Control.Monad.Reader                               (MonadReader,
                                                                     asks)
import           Data.Maybe                                         (catMaybes)
import           Filesystem                                         (createTree,
                                                                     writeFile)
import           Filesystem.Path                                    (directory)
import           Filesystem.Path.CurrentOS                          (decode)
import           Prelude                                            hiding
                                                                     (FilePath,
                                                                     log)

----------------------------------------------------------------------------
-- | Logs that the Node is joining the network and returns a Backend
mkBackend :: NetworkConfig -> IO Backend
mkBackend nc@NetworkConfig {..} = do
  log ("Node joining network on: " ++ show nc)
  initializeBackend hostNetworkConfig portNetworkConfig PN.initRemoteTable

-- | Runs a Process that is a master node
runRequestNode ::
     Int
  -- ^ Number of slave nodes the master should wait for
  -> NetworkConfig
  -> IO ()
runRequestNode waitN nc = do
  backend <- mkBackend nc
  let cfg = AppConfig waitN backend
  flip PN.runProcess (runApp cfg runRequestNode') =<< newLocalNode backend
  runStackBuildT

-- | Internal for master node
runRequestNode' :: App ()
runRequestNode' = do
  log "Searching the Network..."
  pids <- findPids
  logSucc ("Found Nodes: " ++ show pids)
  pDeps <- gatherDeps pids
  log "Finding most compatable node..."
  myDeps <- listDeps
  withTransfer pDeps myDeps retryReq $ \pid (sPort, rPort) -> do
    log $ "Node: " ++ show pid ++ " Is the best match."
    send pid (TransferReq sPort)
    log "Working..."
    receiveF rPort
    logSucc "Transmission complete. All files received. Ready to build."
  mapM_ (`send` Terminate) pids
  where
    retryReq :: SomeException -> App ()
    retryReq _ = logWarn "Slave died. Retrying..." >> runRequestNode'

-- | Creates the Backend and runs a Process that is a Slave node
joinNetwork :: NetworkConfig -> IO ()
joinNetwork = mkBackend >=> newLocalNode >=> flip PN.runProcess joinNetwork'

-- | Internal for slave node
joinNetwork' :: Process ()
joinNetwork' = do
  me <- getSelfPid
  register "slaveNodeS" me
  loop me
  where
    loop me = receiveWait [match $ \req -> receiveReq me req]
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
    receiveReq _ Terminate = log "Received a request to terminate. Bye."

-- | Faciliaties a safe Transfer bewteen nodes
withTransfer ::
     (Exception e, MonadProcess m, MonadCatch m)
  => [ProcessDeps]
  -- ^ Slaves process dependencies
  -> Deps
  -- ^ Masters dependencies
  -> (e -> m ())
  -- ^ Recovery function
  -> (ProcessId -> (SendPort Transfer, ReceivePort Transfer) -> m ())
  -- ^ Action to execute with the linked process and typed-channel
  -> m ()
withTransfer pDeps myDeps retry action =
  case getBestPid pDeps myDeps (Nothing, 0) of
    Just n  -> runAction n `catch` retry
    Nothing -> logWarn "No Nodes share dependencies, aborting."
  where
    runAction n = do
      link n
      chan <- newChan
      linkPort (fst chan)
      action n chan
      unlinkPort (fst chan)
      unlink n

-- | Internal function used to gather ProcessId's
findPids ::
     (MonadMask m, MonadProcess m, MonadReader AppConfig m) => m [ProcessId]
findPids = loop =<< asks nodes
  where
    loop nc = do
      backend <- asks backend
      nids <- liftIO $ findPeers backend 1000000
      pids <-
        bracket (mapM monitorNode nids) (mapM unmonitor) $ \_ -> do
          mapM_ (`whereisRemoteAsync` "slaveNodeS") nids
          catMaybes <$>
            replicateM
              (length nids)
              (receiveWait
                 [ match (\(WhereIsReply "slaveNodeS" mPid) -> pure mPid)
                 , match (\NodeMonitorNotification {} -> pure Nothing)
                 ])
      if length pids == nc
        then pure pids
        else loop nc

-- | Internal function used to get all slaves dependencies
gatherDeps :: (MonadProcess m, MonadMask m) => Network -> m [ProcessDeps]
gatherDeps pids = do
  ping <- Ping <$> getSelfPid
  mapM_ (`send` ping) pids
  bracket (mapM monitor pids) (mapM unmonitor) $ \_ ->
    catMaybes <$>
    replicateM
      (length pids)
      (receiveWait
         [ match $ \(PD pd) -> pure $ Just pd
         , match $ \NodeMonitorNotification {} -> pure Nothing
         ])

-- | Internal function used to reveive the Transfer
receiveF :: MonadProcess m => ReceivePort Transfer -> m ()
receiveF rPort = work =<< receiveChan rPort
  where
    work (TransferInProg (path, file)) = do
      liftIO $ do
        let path' = decode path
        createTree (directory path')
        Filesystem.writeFile path' file
      receiveF rPort
    work TransferDone = pure ()
