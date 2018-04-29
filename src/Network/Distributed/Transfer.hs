{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module:      Network.Distributed.Transfer
-- Copyright:   (c) 2018 Sean McGroarty
-- License:     BSD3
-- Maintainer:  Sean McGroarty <mcgroas@tcd.ie.com>
-- Stability:   experimental
--
module Network.Distributed.Transfer
  ( pipeFiles
  , packageFile
  , producers
  ) where

--------------------------------------------------------------------------------------------
import           Network.Distributed.Types
import           Network.Distributed.Utils

--------------------------------------------------------------------------------------------
import           Control.Applicative       ((<|>))
import           Data.DirStream            (descendentOf)
import           Filesystem                (isFile, readFile)
import           Filesystem.Path           (FilePath)
import           Filesystem.Path.CurrentOS (encode)
import           Pipes
import qualified Pipes.Prelude             as P
import           Pipes.Safe                (MonadMask, MonadSafe, runSafeT)
import           Prelude                   hiding (FilePath, log)

--------------------------------------------------------------------------------------------
-- | Internal function that executes a pipeline of effects
pipeFiles ::
     (MonadMask m, MonadIO m)
  => (Transfer -> m ())
  -- ^ Function that accepts a Transfer and has IO at its base
  -> m ()
pipeFiles action =
  runSafeT $
  runEffect $
  for
    (producers >-> P.filterM (liftIO . isFile) >-> P.chain (log . show) >->
     P.mapM packageFile)
    (lift . lift . action)

-- | Produces output for the pipeline
producers :: MonadSafe m => Pipes.Proxy x' x () FilePath m ()
producers =
  every
    (descendentOf "root/snapshots" <|> descendentOf "root/loaded-snapshot-cache")

-- | Packages a file by reading in its bytes
packageFile :: MonadIO m => FilePath -> m Transfer
packageFile file =
  liftIO $ TransferInProg . (,) (encode file) <$> Filesystem.readFile file
--------------------------------------------------------------------------------------------
