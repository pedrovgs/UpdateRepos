module UpdateRepos where

import           Data.Either
import           Git
import           System

isEnvironmentReady :: IO Bool
isEnvironmentReady = isGitInstalled

listGitRepositories :: FilePath -> IO [FilePath]
listGitRepositories = listDirectoriesRecursive containsGitMetadataDirectory containsOtherVCSMetadataDirectory

updateGitRepository :: FilePath -> IO (Either UpdateRepoError FilePath)
updateGitRepository path = do currentBranch <- getCurrentBranch path
                              updateRepo path currentBranch
