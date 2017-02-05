module System where

import           Control.Monad
import           Data.List
import           System.Directory
import           System.FilePath.Posix

type FilesPredicate = [FilePath] -> Bool

listDirectoriesRecursive :: FilesPredicate -> FilePath -> IO [FilePath]
listDirectoriesRecursive predicate absPath = do let path = appendSeparatorIfNeeded absPath
                                                print ("Checking path " ++ show path)
                                                subDirectories <- listDirectories path
                                                let isGitRepository = predicate subDirectories
                                                if null subDirectories then return []
                                                else do restOfDirectories <- mapM (listDirectoriesRecursive predicate) subDirectories
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

appendSeparatorIfNeeded :: String -> String
appendSeparatorIfNeeded absPath
  | last absPath == pathSeparator = absPath
  | otherwise = absPath ++ [pathSeparator]
