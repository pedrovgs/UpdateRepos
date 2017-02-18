module Git (
    UpdateRepoError(..)
  , UpdateRepoSuccess(..)
  , isGitInstalled
  , getCurrentBranch
  , updateRepo
  , containsGitMetadataDirectory
  , containsOtherVCSMetadataDirectory
)where

import           Data.List.Split
import           System
import           System.Directory
import           System.Exit
import           System.Process

data UpdateRepoError = UpdateRepoError {
  path    :: FilePath
, message :: String
} deriving (Eq, Show)

data UpdateRepoSuccess = UpdateRepoSuccess {
  repo   :: FilePath
, result :: String
} deriving (Eq, Show)

isGitInstalled :: IO Bool
isGitInstalled = do (exitCode, stdOut, stdErr) <- readProcessWithExitCode "git" ["--version"] []
                    return (exitCode == ExitSuccess)

getCurrentBranch :: FilePath ->  IO String
getCurrentBranch path = do let gitRepoPath = appendGitFolder path
                           (exitCode, stdOut, stdErr) <- readProcessWithExitCode "git" ["--git-dir", gitRepoPath, "branch"] []
                           return (extractCurrentBranch stdOut)

updateRepo :: FilePath -> String -> IO (Either UpdateRepoError UpdateRepoSuccess)
updateRepo path branch = do let gitRepoPath = appendGitFolder path
                            readProcessWithExitCode "git" ["--git-dir", "--work-tree", path, gitRepoPath, "fetch", "origin", "master"] []
                            readProcessWithExitCode "git" ["--git-dir", "--work-tree", path, gitRepoPath, "fetch", "origin", "develop"] []
                            (exitCode, stdOut, stdErr) <- readProcessWithExitCode "git" ["--git-dir", gitRepoPath, "--work-tree", path, "pull", "origin", branch, "-n" , "-f"] []
                            if exitCode == ExitSuccess then return (Right (UpdateRepoSuccess path stdOut))
                            else return (Left (UpdateRepoError path stdErr))

containsGitMetadataDirectory :: [FilePath] -> Bool
containsGitMetadataDirectory = any isGitMetadataDirectory

containsOtherVCSMetadataDirectory :: [FilePath] -> Bool
containsOtherVCSMetadataDirectory = any isOtherVCSDierctory

isGitMetadataDirectory :: FilePath -> Bool
isGitMetadataDirectory = contains gitFolder

isOtherVCSDierctory :: FilePath -> Bool
isOtherVCSDierctory = contains ".hg"

contains :: FilePath -> FilePath -> Bool
contains directory path
  | null path = False
  | path == directory = True
  | path == directory ++ separator = True
  | otherwise = contains directory (tail path)

extractCurrentBranch :: String -> String
extractCurrentBranch stdOut = drop 2 $ head selectedBranches
                              where branches = splitOn "\n" stdOut
                                    selectedBranches = filter (\branch -> '*' `elem` branch) branches
appendGitFolder :: String -> String
appendGitFolder path = path ++ gitFolder

gitFolder = ".git"
