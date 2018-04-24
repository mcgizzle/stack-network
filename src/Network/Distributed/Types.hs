{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module Network.Distributed.Types where

import           Control.Distributed.Process.Backend.SimpleLocalnet (Backend)
import           Control.Distributed.Process.Lifted                 (Process,
                                                                     ProcessId,
                                                                     SendPort)
import           Control.Monad.Reader
import           Data.Binary                                        (Binary)
import           Data.ByteString                                    (ByteString)
import           Data.Text                                          (Text)
import           Data.Typeable                                      (Typeable)
import           Filesystem.Path.CurrentOS                          (decode)
import           GHC.Generics                                       (Generic)

---------------------------------------------------------------------------------
data AppConfig = AppConfig
  { nodes   :: Int
  , backend :: Backend
  }

-- | A Monad which wraps Process with ReaderT
type App a = ReaderT AppConfig Process a

runApp :: AppConfig -> App a -> Process a
runApp = flip runReaderT

---------------------------------------------------------------------------------
data NetworkConfig = NetworkConfig
  { hostNetworkConfig :: String
  , portNetworkConfig :: String
  }

instance Show NetworkConfig where
  show NetworkConfig {..} =
    "//" ++ hostNetworkConfig ++ ":" ++ portNetworkConfig

---------------------------------------------------------------------------------
-- | A Node is used for communication
type Node = ProcessId

-- | A collection of Nodes
type Network = [Node]

-- | The dependecies a Node has, represented as a list of Strings
type Deps = [String]

type ProcessDeps = (Deps, ProcessId)

type FileInfo = (Text, ByteString)

---------------------------------------------------------------------------------
data Message
  = Request
  | Response

-- | Only masters can make a Request
data Request
  = Ping ProcessId
  -- ^ Used to transmit masters ProcessId to a slabe
  | TransferReq (SendPort Transfer)
  -- ^ FacilitateS a Transfer
  | Terminate
  -- ^ Terminates a slaves connection to the network
  deriving (Generic, Typeable)

instance Binary Request

-- | Only slaves can issue a Reponse in reply to a Request
data Response
  = PD ProcessDeps
  -- ^ Response to a Ping
  | Transfer
  -- ^ Reponse to a TransferReq
  deriving (Generic, Typeable)

instance Binary Response

data Transfer
  = TransferInProg FileInfo
  -- ^ Transfer In Progress
  | TransferDone
  -- ^ Informs master the Transfer is complete
  deriving (Generic, Typeable)

instance Binary Transfer

instance Show Transfer where
  show (TransferInProg (s, _)) = show $ decode s
  show _                       = "Transfer Complete"

---------------------------------------------------------------------------------
data ProcessError =
  SlaveError

instance Show ProcessError where
  show SlaveError = "Slave died during transfer. Aborting to retry..."
---------------------------------------------------------------------------------
