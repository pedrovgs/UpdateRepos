{-# LANGUAGE ImplicitParams #-}

module UpdateReposSpec where

import           Control.Monad
import           Control.Monad.Free
import           Data.Either
import           Git
import           System
import           System.Exit
import           System.IO.Unsafe
import           SystemFree
import           Test.Hspec
import           Test.QuickCheck
import           UpdateRepos

spec = describe "Git module requirements" $ do
  let ?boolInterpreter = run
  it "returns false if Git is not installed" $ unsafePerformIO $ do
     let ?systemInterpreter = gitInstalledInterpreter False
     result <- isEnvironmentReady
     return (not result)
  it "returns true if Git is installed" $ unsafePerformIO $ do
     let ?systemInterpreter = gitInstalledInterpreter True
     isEnvironmentReady
  it "returns update repo error if there is something wrong" $ unsafePerformIO $ do
    let errorMessage = "Merge conflict error"
    let ?systemInterpreter = executionInterpreter $ executionError errorMessage
    result <- updateGitRepository "/Pedro/Users/Repository/"
    return (result == Left (UpdateRepoError "/Pedro/Users/Repository/" errorMessage))
  it "returns update repo success if result of the pull is correct" $ unsafePerformIO $ do
    let outputMessage = "Already up-to-date."
    let ?systemInterpreter = executionInterpreter $ executionSuccess outputMessage
    result <- updateGitRepository "/Pedro/Users/Repository/"
    return (result == Right (UpdateRepoSuccess "/Pedro/Users/Repository/" outputMessage))

gitInstalledInterpreter :: Bool -> SystemFreeInterpreter (ExitCode, String, String)
gitInstalledInterpreter result (Free (Execute command params t)) =
  run $ t (errorCode, "", "Command not found")
  where errorCode = if result then ExitSuccess else ExitFailure 9
gitInstalledInterpreter _ t = run t

executionInterpreter :: (ExitCode, String, String) -> SystemFreeInterpreter (ExitCode, String, String)
executionInterpreter result (Free (Execute command params t)) =
  run $ t result
executionInterpreter _ t = run t

insertAt :: Int -> a -> [a] -> [a]
insertAt z y xs = as ++ (y:bs)
                  where (as,bs) = splitAt z xs
executionError :: String -> (ExitCode, String, String)
executionError stdError = (ExitFailure (-1), "", stdError)

executionSuccess :: String -> (ExitCode, String, String)
executionSuccess stdOut = (ExitSuccess, stdOut, "")
