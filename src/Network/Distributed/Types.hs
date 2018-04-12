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

type ProcessDeps = (Deps, Node)

type Deps = [String]

data Request
    = Ping ProcessId
    | Transfer (SendPort TransferData)
    | Terminate
    deriving (Generic, Typeable)

instance Binary Request

data Response
    = PD ProcessDeps
    | TransferData FileInfo
    deriving (Generic, Typeable)

instance Binary Response

type FileInfo = (ByteString, ByteString)

data TransferData
    = TransferInProg FileInfo
    | TransferDone
    deriving (Generic, Typeable)

instance Show TransferData where
    show (TransferInProg (s, _)) = show $ decode s
    show _                       = "Transfer Complete"

instance Binary TransferData
