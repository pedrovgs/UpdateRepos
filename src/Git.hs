module Git where

import           Data.List.Split
import           System
import           System.Directory
import           System.Exit
import           System.Process

data UpdateRepoError = UpdateRepoError {
  message:: String
} deriving (Eq, Show)

isGitInstalled :: IO Bool
isGitInstalled = do (exitCode, stdOut, stdErr) <- readProcessWithExitCode "git" ["--version"] []
                    return (exitCode == ExitSuccess)

getCurrentBranch :: FilePath ->  IO String
getCurrentBranch path = do let gitRepoPath = path ++ ".git"
                           (exitCode, stdOut, stdErr) <- readProcessWithExitCode "git" ["--git-dir", gitRepoPath, "branch"] []
                           return (extractCurrentBranch stdOut)

containsGitMetadataDirectory :: [FilePath] -> Bool
containsGitMetadataDirectory = any isGitMetadataDirectory

containsOtherVCSMetadataDirectory :: [FilePath] -> Bool
containsOtherVCSMetadataDirectory = any isOtherVCSDierctory

isGitMetadataDirectory :: FilePath -> Bool
isGitMetadataDirectory = contains ".git"

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
