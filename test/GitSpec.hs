{-# LANGUAGE ImplicitParams #-}

module GitSpec where

import           Control.Monad
import           Control.Monad.Free
import           Git
import           System.Exit
import           SystemFree
import           Test.Hspec
import           Test.QuickCheck


gitIsNotInstalledInterpreter :: SystemFreeInterpreter (ExitCode, String, String)
gitIsNotInstalledInterpreter (Free (Execute command params t)) =
  run $ t (errorCode, "", "Command not found")
  where errorCode = ExitFailure 9
gitIsNotInstalledInterpreter t = run t


spec = describe "Git module requirements" $ do
  it "returns false if Git is not installed" $ do
     let ?boolInterpreter = run
         ?systemInterpreter = gitIsNotInstalledInterpreter
     return (not isGitInstalled)
  it "returns true if Git is installed" $ do
    let ?boolInterpreter = run
        ?systemInterpreter = gitIsNotInstalledInterpreter
    return isGitInstalled
