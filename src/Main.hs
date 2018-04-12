{-# LANGUAGE LambdaCase #-}

module Main where

import           System.Environment  (getArgs)

import           Network.Distributed

main :: IO ()
main = do
    getArgs >>= \case
        ["build"] -> runRequestNode =<< parseNetConfig
        ["join"] -> joinNetwork =<< parseNetConfig
        _ -> putStrLn "Bar args, Maysh."
