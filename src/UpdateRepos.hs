{-# LANGUAGE ImplicitParams #-}

module UpdateRepos where

import           Data.Either
import           Git
import           System
import           System.Exit
import           SystemFree

isEnvironmentReady :: (?systemInterpreter :: SystemFreeInterpreter (ExitCode, String, String)) => IO Bool
isEnvironmentReady = isGitInstalled

listGitRepositories ::(?boolInterpreter :: SystemFreeInterpreter Bool) => FilePath -> IO [FilePath]
listGitRepositories = listDirectoriesRecursive containsGitMetadataDirectory containsOtherVCSMetadataDirectory

updateGitRepository ::(?systemInterpreter :: SystemFreeInterpreter (ExitCode, String, String)) => FilePath -> IO (Either UpdateRepoError UpdateRepoSuccess)
updateGitRepository path = do currentBranch <- getCurrentBranch path
                              updateRepo path currentBranch
