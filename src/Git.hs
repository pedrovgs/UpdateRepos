module Git where

containsGitMetadataDirectory :: [FilePath] -> Bool
containsGitMetadataDirectory = any isGitMetadataDirectory

isGitMetadataDirectory :: FilePath -> Bool
isGitMetadataDirectory ""      = False
isGitMetadataDirectory ".git"  = True
isGitMetadataDirectory ".git/" = True
isGitMetadataDirectory (x:xs)  = isGitMetadataDirectory xs
