module Main where

import           Control.Monad
import           Data.List
import           Debug.Trace
import           System.Directory
import           System.FilePath.Posix
import           UpdateRepos

main :: IO ()
main = do currentDir <- getCurrentDirectory
          -- TODO: Just if
          let path = currentDir ++ [pathSeparator]
          gitDirectories <- listGitRepositories path
          print $ "Current directory = " ++ show path
          print $ "List of GIT directories = " ++ show gitDirectories
          print "We are done!"

listGitRepositories :: FilePath -> IO [FilePath]
listGitRepositories path = do subDirectories <- listDirectories path
                              let isGitRepository = containsGitDirectory subDirectories
                              if null subDirectories then return []
                              else do restOfDirectories <- mapM listGitRepositories subDirectories
                                      if isGitRepository then do let restOfGitDirectories = concat restOfDirectories
                                                                 return (path : restOfGitDirectories)
                                      else trace (show restOfDirectories) $ return (concat restOfDirectories)

listDirectories :: FilePath -> IO [FilePath]
listDirectories path = do subFilesAndSubDirectories <- listDirectory path
                          directories <- filterM (isDirectory path) subFilesAndSubDirectories
                          return (map (path ++ ) directories)

containsGitDirectory :: [FilePath] -> Bool
containsGitDirectory = any isGitDirectory

isDirectory :: FilePath -> FilePath -> IO Bool
isDirectory origin dirName = do let absolutePath = origin ++ dirName
                                let isGitDir = isGitDirectory absolutePath
                                let isHidden = "." `isPrefixOf` absolutePath
                                isDirectory <- doesDirectoryExist absolutePath
                                return (isGitDir || (isDirectory && not isHidden))

isGitDirectory :: FilePath -> Bool
isGitDirectory ".git" = True
isGitDirectory _      = False
