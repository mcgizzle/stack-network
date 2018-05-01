{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

-- |
-- Module:      Network.Distributed.Types
-- Copyright:   (c) 2018 Sean McGroarty
-- License:     BSD3
-- Maintainer:  Sean McGroarty <mcgroas@tcd.ie.com>
-- Stability:   experimental
--
module Network.Distributed.Types where

import           Control.Distributed.Process.Backend.SimpleLocalnet (Backend)
import           Control.Distributed.Process.Lifted                 (Process,
                                                                     ProcessId,
                                                                     SendPort)
import           Control.Monad.Reader                               (ReaderT,
                                                                     runReaderT)
import           Data.Binary                                        (Binary)
import           Data.ByteString                                    (ByteString)
import           Data.Typeable                                      (Typeable)
import           GHC.Generics                                       (Generic)

---------------------------------------------------------------------------------
-- | Configuration for the master node
data AppConfig = AppConfig
  { nodes   :: Int
  -- ^ Number of slave nodes the master should wait for before attempting a build
  , backend :: Backend
  }

-- | NetProc Monad
--
-- Wraps Process with ReaderT
type NetProc a = ReaderT AppConfig Process a

-- | Run the 'App' Monad
runNetProc :: AppConfig -> NetProc a -> Process a
runNetProc = flip runReaderT

---------------------------------------------------------------------------------
-- | Host and Port of a 'Node'
data NetworkConfig = NetworkConfig
  { hostNetworkConfig :: String
  , portNetworkConfig :: String
  }

instance Show NetworkConfig where
  show NetworkConfig {..} =
    "//" ++ hostNetworkConfig ++ ":" ++ portNetworkConfig

---------------------------------------------------------------------------------
-- | A 'Node' is used for communication
type Node = ProcessId

-- | A collection of 'Node's
type Network = [Node]

-- | The dependecies a 'Node' has, represented as a list of Strings
type Deps = [String]

-- | A Node and its dependencies
type ProcessDeps = (Deps, ProcessId)

-- | Tuple of file name and bytes
type FileInfo = (ByteString, ByteString)

---------------------------------------------------------------------------------
-- | All Types used messaging are derived from a Message
data Message
  = Request
  | Response

-- | Only masters can make a Request
data Request
  = Ping ProcessId
  -- ^ Used to transmit masters 'ProcessId' to a slave
  | TransferReq (SendPort Transfer)
  -- ^ Facilitates a 'Transfer'
  | Terminate
  -- ^ Terminates a slaves connection to the network
  deriving (Generic, Typeable)

instance Binary Request

-- | Only slaves can issue a Reponse in reply to a 'Request'
data Response
  = PD ProcessDeps
  -- ^ Response to a Ping
  | Transfer
  -- ^ Reponse to a TransferReq
  deriving (Generic, Typeable)

instance Binary Response

-- | Data-Type used in a Transfer
data Transfer
  = TransferInProg FileInfo
  -- ^ Transfer In Progress
  | TransferDone
  -- ^ Informs master the Transfer is complete
  deriving (Generic, Typeable)

instance Binary Transfer

---------------------------------------------------------------------------------
-- | Errors covering a 'Process' failing
data ProcessError =
  SlaveError

instance Show ProcessError where
  show SlaveError = "Slave died during transfer. Aborting to retry..."
---------------------------------------------------------------------------------
