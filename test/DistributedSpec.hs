{-# LANGUAGE OverloadedStrings #-}

module DistributedSpec where

import           System.Directory (getCurrentDirectory)
import           System.Exit      (ExitCode (..))
import           System.Process
import           Test.Hspec

spec :: Spec
spec = do
  describe "Test using docker-compose" $ do
    it "Runs two containers with one dependency" $ do
      res <- runDocker
      clearDocker
      res `shouldBe` True

runDocker :: IO Bool
runDocker = do
  dir <- getCurrentDirectory
  (exit_code, _, _) <-
    readCreateProcessWithExitCode
      (proc "docker-compose" ["up", "--exit-code-from", "master"])
      {cwd = Just $ dir ++ "/test"}
      ""
  case exit_code of
    ExitSuccess   -> pure True
    ExitFailure e -> print e >> pure False

clearDocker :: IO ()
clearDocker = do
  dir <- getCurrentDirectory
  createProcess (proc "docker-compose" ["down"]) {cwd = Just $ dir ++ "/test"}
  pure ()
