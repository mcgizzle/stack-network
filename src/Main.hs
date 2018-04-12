{-# LANGUAGE LambdaCase #-}

module Main where

import           System.Environment  (getArgs)

import           Network.Distributed

main :: IO ()
main = do
  getArgs >>= \case
    ["build"] -> runRequestNode =<< parseNetConfig
    ["join"] -> runStackBuildT >> parseNetConfig >>= joinNetwork
