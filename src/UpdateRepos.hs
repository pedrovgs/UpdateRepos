module UpdateRepos where

import           Git
import           System

listGitRepositories :: FilePath -> IO [FilePath]
listGitRepositories = listDirectoriesRecursive containsGitMetadataDirectory
