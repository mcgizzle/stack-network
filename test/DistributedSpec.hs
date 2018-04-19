{-# LANGUAGE OverloadedStrings #-}

module DistributedSpec where

import           DockerCompose

import           Data.ByteString  (ByteString)
import qualified Data.ByteString  as BS
import           GHC.IO.Handle
import           System.Directory (getCurrentDirectory)
import           System.Exit      (ExitCode (..))
import           System.IO.Temp   (withTempFile)
import           System.Process
import           Test.Hspec

type Test = (ByteString, String)

tests :: [Test]
tests =
  [ (simpleYaml, "Runs two containers with one dependency")
  , (threeNYaml, "Runs three containers with one dependency each")
  , (fourNYaml, "Runs four containers with multiple dependencies")
  ]

spec :: Spec
spec = describe "Test using docker-compose" $ mapM_ (uncurry runSpec) tests

runSpec :: ByteString -> String -> SpecWith ()
runSpec file info =
  it info $ do
    dir <- getCurrentDirectory
    withTempFile dir "temp.yml" $ \fp h -> do
      hSetBuffering h NoBuffering
      BS.hPut h file
      res <- execDocker fp
      res `shouldBe` True

execDocker :: FilePath -> IO Bool
execDocker dc = runDocker dc <* clearDocker dc

runDocker :: FilePath -> IO Bool
runDocker file = do
  (_, _, _, ph) <-
    createProcess (proc "docker-compose" ["-f", file, "up", "--force-recreate"])
  exit_code <- waitForProcess ph
  case exit_code of
    ExitSuccess   -> pure True
    ExitFailure e -> print e >> pure False

clearDocker :: FilePath -> IO ()
clearDocker file = do
  (_, _, _, ph) <-
    createProcess
      (proc "docker-compose" ["-f", file, "down"])
      {std_out = NoStream, std_err = NoStream}
  _ <- waitForProcess ph
  pure ()
