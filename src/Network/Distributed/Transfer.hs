{-# LANGUAGE OverloadedStrings #-}

module Network.Distributed.Transfer where

import           Network.Distributed.Types
import           Network.Distributed.Utils

import           Control.Applicative
import           Data.DirStream
import           Filesystem
import           Filesystem.Path           (FilePath)
import           Filesystem.Path.CurrentOS (encode)
import           Pipes
import qualified Pipes.Prelude             as P
import           Pipes.Safe                (MonadMask, MonadSafe, runSafeT)
import           Prelude                   hiding (FilePath, log)

pipeFiles :: (MonadMask m, MonadIO m) => (Transfer -> m ()) -> m ()
pipeFiles action =
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
  liftIO $ TransferInProg . (,) (encode file) <$> Filesystem.readFile file
