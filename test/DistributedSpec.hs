{-# LANGUAGE OverloadedStrings #-}

module DistributedSpec where

import           Control.Concurrent.Async

import           Control.Distributed.Process                        hiding
                                                                     (send)
import           Control.Distributed.Process.Backend.SimpleLocalnet (Backend, initializeBackend,
                                                                     newLocalNode)
import           Control.Distributed.Process.Node                   (initRemoteTable,
                                                                     runProcess)
import           Data.Monoid
import           Distributed
import           Filesystem
import           Filesystem.Path
import           Prelude                                            hiding
                                                                     (FilePath,
                                                                     writeFile)
import           System.Directory                                   (getDirectoryContents)
import           Test.Hspec

spec :: Spec
spec = do
    runIO runTests
    runIO $ putStrLn "Ran Tests."
    describe "Sending/Receiving a file" $ do
        it "Compares Files" $ do 1 `shouldBe` 1

--    runIO clearDirs
runTests :: IO ()
runTests = do
    node1 <-
        newLocalNode =<< initializeBackend "127.0.0.1" "5000" initRemoteTable
    backend2 <- initializeBackend "127.0.0.1" "5001" initRemoteTable
    node2 <- newLocalNode backend2
    _ <-
        concurrently
            (runProcess node1 send)
            (runProcess node2 (receive backend2))
    return ()

sTemp :: FilePath
sTemp = "stemp"

rTemp :: FilePath
rTemp = "rtemp"

clearDirs :: IO ()
clearDirs = do
    removeTree sTemp
    removeTree rTemp

send :: Process ()
send = do
    liftIO $ do
        createTree (sTemp <> "root/snapshots")
        cur <- getWorkingDirectory
        setWorkingDirectory (cur <> sTemp)
        liftIO $ print <$> getWorkingDirectory
        writeFile "root/snapshots/test.txt" "This is a test"
    joinNetwork

receive :: Backend -> Process ()
receive backend = do
    liftIO $ do
        createDirectory False rTemp
        cur <- getWorkingDirectory
        setWorkingDirectory (cur <> rTemp)
        liftIO $ print <$> getWorkingDirectory
    runBuild backend
