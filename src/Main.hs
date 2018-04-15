{-# LANGUAGE LambdaCase #-}

module Main where

import           System.Environment  (getArgs)

import           Network.Distributed

main :: IO ()
main =
  getArgs >>= \case
    ["build"] -> runRequestNode =<< parseNetConfig
    ["join"] -> runStackBuildT >> parseNetConfig >>= joinNetwork
    _ -> logWarn "Bad arguments.\n COMMAND [join | build]"
