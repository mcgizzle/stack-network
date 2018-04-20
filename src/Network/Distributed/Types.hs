{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module Network.Distributed.Types where

import           Control.Distributed.Process                        (Process,
                                                                     ProcessId,
                                                                     SendPort)
import           Control.Distributed.Process.Backend.SimpleLocalnet (Backend)
import           Control.Monad.Reader
import           Data.Binary                                        (Binary)
import           Data.ByteString                                    (ByteString)
import           Data.Text                                          (Text)
import           Data.Typeable                                      (Typeable)
import           Filesystem.Path.CurrentOS                          (decode)
import           GHC.Generics                                       (Generic)

data AppConfig = AppConfig
  { nodes   :: Int
  , backend :: Backend
  }

type App a = ReaderT AppConfig Process a

runApp :: AppConfig -> App a -> Process a
runApp = flip runReaderT

data NetworkConfig = NetworkConfig
  { hostNetworkConfig :: String
  , portNetworkConfig :: String
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

type ProcessDeps = (Deps, ProcessId)

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

data ProcessError =
  SlaveError

instance Show ProcessError where
  show SlaveError = "Slave died during transfer. Aborting to retry..."
