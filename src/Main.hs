-- |
-- Module:      Main
-- Copyright:   (c) 2018 Sean McGroarty
-- License:     BSD3
-- Maintainer:  Sean McGroarty <mcgroas@tcd.ie.com>
-- Stability:   experimental
--
module Main
  ( main
  ) where

-------------------------------------------------------------------------------------------
import           Network.Distributed

-------------------------------------------------------------------------------------------
import           Control.Monad       (join)
import           Data.Semigroup      ((<>))
import           Options.Applicative

newtype Opts = Opts
  { waitNodes :: Int
  }

optParser :: Parser Opts
optParser =
  Opts <$>
  option
    auto
    (long "nodes" <> short 'n' <>
     help "How many nodes the Master should wait for before beginning build" <>
     showDefault <>
     value 1 <>
     metavar "INT")

description :: InfoMod a
description =
  fullDesc <>
  progDesc "stack-network interfaces with Stack to create a distributed version" <>
  header "stack-network"

commands :: Parser (IO ())
commands =
  subparser $
  command "build" (info (runBuild <$> optParser) description) <>
  command "join" (info (pure runJoin) description)
  where
    runJoin = runStackBuildT >> parseNetConfig >>= joinNetwork
    runBuild opts = runRequestNode (waitNodes opts) =<< parseNetConfig

-- | Main
main :: IO ()
main = join $ execParser (info (commands <**> helper) description)
