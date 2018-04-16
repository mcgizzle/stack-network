{-# LANGUAGE LambdaCase #-}

module Main where

import           Control.Monad       (join)
import           Data.Semigroup      ((<>))
import           Network.Distributed
import           Options.Applicative

data Opts = Opts
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
commands = subparser $ buildCmd <> joinCmd

buildCmd :: Mod CommandFields (IO ())
buildCmd = command "build" (info (runBuild <$> optParser) description)

joinCmd :: Mod CommandFields (IO ())
joinCmd = command "join" (info (pure runJoin) description)

runJoin :: IO ()
runJoin = runStackBuildT >> parseNetConfig >>= joinNetwork

runBuild :: Opts -> IO ()
runBuild opts = runRequestNode (waitNodes opts) =<< parseNetConfig

main :: IO ()
main = join $ execParser (info (commands <**> helper) description)
