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
import           Control.Monad.Catch                                (SomeException (..),
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

runRequestNode :: Int -> NetworkConfig -> IO ()
runRequestNode waitN NetworkConfig {..} = do
  backend <-
    initializeBackend hostNetworkConfig portNetworkConfig PN.initRemoteTable
  node <- newLocalNode backend
  PN.runProcess node (runRequestNode' backend waitN)
  runStackBuildT

runRequestNode' :: Backend -> Int -> Process ()
runRequestNode' backend waitN = do
  log "Searching the Network..."
  pids <- findPids backend waitN
  logSucc $ "Found Nodes: " ++ show pids
  pDeps <- gatherDeps pids
  log "Finding most compatable node..."
  myDeps <- listDeps
  case getBestPid pDeps myDeps (Nothing, 0) of
    Nothing -> logWarn "No Nodes share dependencies, aborting."
    Just n  -> runTransfer n `catch` abort
  mapM_ (flip send Terminate) pids
  where
    runTransfer n = do
      link n
      log $ "Node: " ++ show n ++ " Is the best match."
      (sPort, rPort) <- newChan
      linkPort sPort
      send n (TransferReq sPort)
      log "Working..."
      receiveF rPort
      unlinkPort sPort
      unlink n
      logSucc "Transmission complete. All files received. Ready to build."
    abort :: SomeException -> Process ()
    abort _ = logWarn "Slave died. Retrying..." >> runRequestNode' backend waitN

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

findPids :: Backend -> Int -> Process [ProcessId]
findPids backend = loop
  where
    loop nc = do
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
