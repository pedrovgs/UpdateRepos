module Main where

import           Control.Monad
import           Data.List
import           Data.String
import           System.Directory
import           System.FilePath.Posix
import           UpdateRepos

main :: IO ()
main = do currentDir <- getCurrentDirectory
          gitDirectories <- listGitRepositories currentDir
          print $ "Current directory = " ++ show currentDir
          print $ "List of GIT directories = " ++ show gitDirectories
          print "We are done!"

listGitRepositories :: FilePath -> IO [FilePath]
listGitRepositories absPath = do let path = appendSeparatorIfNeeded absPath
                                 print ("Checking path " ++ show path)
                                 subDirectories <- listDirectories path
                                 let isGitRepository = containsGitMetadataDirectory subDirectories
                                 if null subDirectories then return []
                                 else do restOfDirectories <- mapM listGitRepositories subDirectories
                                         if isGitRepository then do let restOfGitDirectories = concat restOfDirectories
                                                                    return (path : restOfGitDirectories)
                                         else return (concat restOfDirectories)

listDirectories :: FilePath -> IO [FilePath]
listDirectories path = do subFilesAndSubDirectories <- listDirectory path
                          let absolutePath = map (\sub -> path ++ sub) subFilesAndSubDirectories
                              absPathsWithSeparators = map appendSeparatorIfNeeded absolutePath
                          filterM isDirectory absPathsWithSeparators

isDirectory :: FilePath -> IO Bool
isDirectory absolutePath = do let isHidden = "." `isPrefixOf` absolutePath
                              isDirectory <- doesDirectoryExist absolutePath
                              return (isDirectory && not isHidden)

containsGitMetadataDirectory :: [FilePath] -> Bool
containsGitMetadataDirectory = any isGitMetadataDirectory

isGitMetadataDirectory :: FilePath -> Bool
isGitMetadataDirectory ""      = False
isGitMetadataDirectory ".git"  = True
isGitMetadataDirectory ".git/" = True
isGitMetadataDirectory (x:xs)  = isGitMetadataDirectory xs

appendSeparatorIfNeeded :: String -> String
appendSeparatorIfNeeded absPath
  | last absPath == pathSeparator = absPath
  | otherwise = absPath ++ [pathSeparator]
