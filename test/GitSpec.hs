{-# LANGUAGE ImplicitParams #-}

module GitSpec where

import           Control.Monad
import           Control.Monad.Free
import           Git
import           System.Exit
import           System.IO.Unsafe
import           SystemFree
import           Test.Hspec
import           Test.QuickCheck

gitInstalledInterpreter :: Bool -> SystemFreeInterpreter (ExitCode, String, String)
gitInstalledInterpreter result (Free (Execute command params t)) =
  run $ t (errorCode, "", "Command not found")
  where errorCode = if result then ExitSuccess else ExitFailure 9
gitInstalledInterpreter _ t = run t


spec = describe "Git module requirements" $ do
  it "returns false if Git is not installed" $ unsafePerformIO $ do
     let ?boolInterpreter = run
         ?systemInterpreter = gitInstalledInterpreter False
     result <- isGitInstalled
     return (not result)
  it "returns true if Git is installed" $ unsafePerformIO $ do
     let ?boolInterpreter = run
         ?systemInterpreter = gitInstalledInterpreter True
     isGitInstalled
