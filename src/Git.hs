module Git where

import           System

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
