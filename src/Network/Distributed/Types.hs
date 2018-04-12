{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE RecordWildCards    #-}

module Network.Distributed.Types where

import           Control.Distributed.Process (ProcessId, SendPort)
import           Data.Binary                 (Binary)
import           Data.ByteString             (ByteString)
import           Data.Text                   (Text)
import           Data.Typeable               (Typeable)
import           Filesystem.Path.CurrentOS   (decode)
import           GHC.Generics                (Generic)

data NetworkConfig = NetworkConfig
  { hostNetworkConfig :: String
  , portNetworkConfig :: String
  , pathNetworkConfig :: String
  }

instance Show NetworkConfig where
  show NetworkConfig {..} =
    "//" ++ hostNetworkConfig ++ ":" ++ portNetworkConfig

type Node = ProcessId

type Network = [Node]

data Message
  = Request
  | Response

data Request
  = Ping ProcessId
  | TransferReq (SendPort Transfer)
  | Terminate
  deriving (Generic, Typeable)

instance Binary Request

type Deps = [String]

type ProcessDeps = (Deps, Node)

data Response
  = PD ProcessDeps
  | Transfer
  deriving (Generic, Typeable)

instance Binary Response

type FileInfo = (Text, ByteString)

data Transfer
  = TransferInProg FileInfo
  | TransferDone
  deriving (Generic, Typeable)

instance Binary Transfer

instance Show Transfer where
  show (TransferInProg (s, _)) = show $ decode s
  show _                       = "Transfer Complete"
