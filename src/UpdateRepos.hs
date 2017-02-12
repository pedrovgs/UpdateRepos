{-# LANGUAGE ImplicitParams #-}

module UpdateRepos where

import           Data.Either
import           Git
import           System
import SystemFree


isEnvironmentReady :: IO Bool
isEnvironmentReady = isGitInstalled

listGitRepositories ::(?systemInterpreter :: SystemFreeInterpreter Bool) => FilePath -> IO [FilePath]
listGitRepositories = listDirectoriesRecursive containsGitMetadataDirectory containsOtherVCSMetadataDirectory

updateGitRepository :: FilePath -> IO (Either UpdateRepoError UpdateRepoSuccess)
updateGitRepository path = do currentBranch <- getCurrentBranch path
                              updateRepo path currentBranch
