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
  it "returns update repo error if there is something wrong" $ unsafePerformIO $ do
    let errorMessage = "Merge conflict error"
    let ?systemInterpreter = executionInterpreter $ executionError errorMessage
    result <- updateRepo "/Pedro/Users/Repository/" "currentBanch"
    return (result == Left (UpdateRepoError "/Pedro/Users/Repository/" errorMessage))
  it "returns update repo success if result of the pull is correct" $ unsafePerformIO $ do
    let outputMessage = "Already up-to-date."
    let ?systemInterpreter = executionInterpreter $ executionSuccess outputMessage
    result <- updateRepo "/Pedro/Users/Repository/" "currentBanch"
    return (result == Right (UpdateRepoSuccess "/Pedro/Users/Repository/" outputMessage))
  it "returns true if the list of file paths contains a .git folder" $
    property $ prop_ContainsAGitDirectoryIfThereIsAGitMetadataFolder
  it "returns false if the list of file paths does contains a .git folder" $
    property $ prop_DoesNotContainsAGitDirectoryIfThereIsAGitMetadataFolder


prop_BranchMarkedWithAsteriskRepresentsTheCurrentBranch :: String -> Property
prop_BranchMarkedWithAsteriskRepresentsTheCurrentBranch branch =
  forAll (repositoryBranches branch) (\genBranches -> unsafePerformIO $ do
    let ?systemInterpreter = currentBranchInterpreter genBranches
    result <- getCurrentBranch "anyPath"
    return (result == branch))

prop_ContainsAGitDirectoryIfThereIsAGitMetadataFolder :: Property
prop_ContainsAGitDirectoryIfThereIsAGitMetadataFolder =
  forAll foldersIncludingGit containsGitMetadataDirectory

prop_DoesNotContainsAGitDirectoryIfThereIsAGitMetadataFolder :: Property
prop_DoesNotContainsAGitDirectoryIfThereIsAGitMetadataFolder =
  forAll folders (\folders -> not $ containsGitMetadataDirectory folders)

foldersIncludingGit :: Gen [FilePath]
foldersIncludingGit = foldersIncludingPath ".git"

foldersIncludingPath :: FilePath -> Gen [FilePath]
foldersIncludingPath path =
  do position <- arbitrary
     genFolders <- folders
     let arrayPosition = abs position `mod` length genFolders
         folders = insertAt position path genFolders
     return (folders)

folders :: Gen [FilePath]
folders = listOf folder

folder :: Gen FilePath
folder = listOf $ choose ('a', 'z')

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
