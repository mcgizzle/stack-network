{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Network.Distributed.Process where

import           Network.Distributed.Transfer
import           Network.Distributed.Types
import           Network.Distributed.Utils

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Catch
import           Prelude                                            hiding
                                                                     (FilePath,
                                                                     log)

import           Control.Distributed.Process.Backend.SimpleLocalnet
import           Control.Distributed.Process.Lifted                 hiding
                                                                     (bracket,
                                                                     catch)
import           Control.Distributed.Process.Lifted.Class
import qualified Control.Distributed.Process.Node.Lifted            as PN

import           Data.Maybe                                         (catMaybes)

import           Filesystem
import           Filesystem.Path                                    (directory)
import           Filesystem.Path.CurrentOS                          (decode)

import           Control.Monad.Reader

runRequestNode :: Int -> NetworkConfig -> IO ()
runRequestNode waitN NetworkConfig {..} = do
  backend <-
    initializeBackend hostNetworkConfig portNetworkConfig PN.initRemoteTable
  node <- newLocalNode backend
  let cfg = AppConfig waitN backend
  PN.runProcess node (runApp cfg runRequestNode')
  runStackBuildT

runRequestNode' :: App ()
runRequestNode' = do
  log "Searching the Network..."
  pids <- findPids
  logSucc $ "Found Nodes: " ++ show pids
  pDeps <- gatherDeps pids
  log "Finding most compatable node..."
  myDeps <- listDeps
  withTransfer pDeps myDeps retryReq $ \pid (sPort, rPort) -> do
    log $ "Node: " ++ show pid ++ " Is the best match."
    send pid (TransferReq sPort)
    log "Working..."
    receiveF rPort
    logSucc "Transmission complete. All files received. Ready to build."
  mapM_ (flip send Terminate) pids
  where
    retryReq :: SomeException -> App ()
    retryReq _ = logWarn "Slave died. Retrying..." >> runRequestNode'

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

withTransfer ::
     (Exception e, MonadProcess m, MonadCatch m)
  => [ProcessDeps]
  -> Deps
  -> (e -> m ())
  -> (ProcessId -> (SendPort Transfer, ReceivePort Transfer) -> m ())
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

findPids ::
     (MonadMask m, MonadProcess m, MonadReader AppConfig m) => m [ProcessId]
findPids = loop =<< asks nodes
  where
    loop nc = do
      backend <- asks backend
      nids <- liftIO $ findPeers backend 1000000
      pids <-
        bracket (mapM monitorNode nids) (mapM unmonitor) $ \_ -> do
          forM_ nids $ \n -> whereisRemoteAsync n "slaveNodeS"
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

gatherDeps :: (MonadProcess m, MonadMask m) => Network -> m [ProcessDeps]
gatherDeps pids = do
  me <- getSelfPid
  mapM_ (flip send (Ping me)) pids
  bracket (mapM monitor pids) (mapM unmonitor) $ \_ ->
    catMaybes <$>
    replicateM
      (length pids)
      (receiveWait
         [ match $ \(PD pd) -> pure $ Just pd
         , match $ \NodeMonitorNotification {} -> pure Nothing
         ])

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
