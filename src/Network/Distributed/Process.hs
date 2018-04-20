{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Distributed.Process where

import           Network.Distributed.Transfer
import           Network.Distributed.Types
import           Network.Distributed.Utils

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Catch                                (Exception, SomeException (..),
                                                                     bracket,
                                                                     catch)
import           Prelude                                            hiding
                                                                     (FilePath,
                                                                     log)

import           Control.Distributed.Process                        hiding
                                                                     (bracket,
                                                                     catch)
import           Control.Distributed.Process.Backend.SimpleLocalnet
import qualified Control.Distributed.Process.Node                   as PN

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
  pDeps <- liftP $ gatherDeps pids
  log "Finding most compatable node..."
  myDeps <- liftP listDeps
  withTransfer pDeps myDeps retryReq $ \pid (sPort, rPort) -> do
    log $ "Node: " ++ show pid ++ " Is the best match."
    liftP $ send pid (TransferReq sPort)
    log "Working..."
    liftP $ receiveF rPort
    logSucc "Transmission complete. All files received. Ready to build."
  liftP $ mapM_ (flip send Terminate) pids
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
     Exception e
  => [ProcessDeps]
  -> Deps
  -> (e -> App ())
  -> (ProcessId -> (SendPort Transfer, ReceivePort Transfer) -> App ())
  -> App ()
withTransfer pDeps myDeps retry action =
  case getBestPid pDeps myDeps (Nothing, 0) of
    Just n  -> runAction n `catch` retry
    Nothing -> logWarn "No Nodes share dependencies, aborting."
  where
    runAction n = do
      chan <-
        liftP $ do
          link n
          chan <- newChan
          linkPort $ fst chan
          pure chan
      action n chan
      liftP $ do
        unlinkPort $ fst chan
        unlink n

findPids :: App [ProcessId]
findPids = loop =<< asks nodes
  where
    loop nc = do
      backend <- asks backend
      nids <- liftIO $ findPeers backend 1000000
      pids <-
        liftP $
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

gatherDeps :: Network -> Process [ProcessDeps]
gatherDeps pids = do
  me <- getSelfPid
  mapM_ (flip send (Ping me)) pids
  replicateM (length pids) (receiveWait [match $ \(PD pd) -> pure pd])

receiveF :: ReceivePort Transfer -> Process ()
receiveF rPort = work =<< receiveChan rPort
  where
    work :: Transfer -> Process ()
    work (TransferInProg (path, file)) = do
      liftIO $ do
        let path' = decode path
        createTree (directory path')
        Filesystem.writeFile path' file
      receiveF rPort
    work TransferDone = pure ()
