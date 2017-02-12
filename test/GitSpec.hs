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

spec = describe "Git module requirements" $ do
  let ?boolInterpreter = run
  it "returns false if Git is not installed" $ unsafePerformIO $ do
     let ?systemInterpreter = gitInstalledInterpreter False
     result <- isGitInstalled
     return (not result)
  it "returns true if Git is installed" $ unsafePerformIO $ do
     let ?systemInterpreter = gitInstalledInterpreter True
     isGitInstalled
  it "returns the current branch obtained from the list of branches git returns" $
    property $ prop_BranchMarkedWithAsteriskRepresentsTheCurrentBranch "any-branch"

prop_BranchMarkedWithAsteriskRepresentsTheCurrentBranch :: String -> Property
prop_BranchMarkedWithAsteriskRepresentsTheCurrentBranch branch =
  forAll (repositoryBranches branch) (\genBranches -> unsafePerformIO $ do
    let ?systemInterpreter = currentBranchInterpreter genBranches
    result <- getCurrentBranch "anyPath"
    return (result == branch))

repositoryBranches :: String -> Gen String
repositoryBranches branch =
  do position <- arbitrary
     genBranches <- listOf branchName
     let currentBranch = "* " ++ branch
         arrayPosition = abs position `mod` length genBranches
         branches = insertAt position currentBranch genBranches
     return (unlines branches)

branchName :: Gen String
branchName = listOf $ choose ('a', '9')

currentBranchInterpreter :: String -> SystemFreeInterpreter (ExitCode, String, String)
currentBranchInterpreter branches (Free (Execute command params t)) =
  run $ t (ExitSuccess, branches, "")
currentBranchInterpreter _ t = run t

gitInstalledInterpreter :: Bool -> SystemFreeInterpreter (ExitCode, String, String)
gitInstalledInterpreter result (Free (Execute command params t)) =
  run $ t (errorCode, "", "Command not found")
  where errorCode = if result then ExitSuccess else ExitFailure 9
gitInstalledInterpreter _ t = run t

insertAt :: Int -> a -> [a] -> [a]
insertAt z y xs = as ++ (y:bs)
                  where (as,bs) = splitAt z xs
