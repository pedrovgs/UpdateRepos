module Main where

import           Control.Monad
import           Data.ByteString    (ByteString)
import           Data.Maybe
import qualified Data.Text          as T
import           Data.Text.Encoding (encodeUtf8)
import           Git
import           System.Directory
import           UpdateRepos

main :: IO ()
main = do currentDir <- getCurrentDirectory
          listOfFilesAndDirectories <- listDirectory currentDir
          justDirectories <- filterM doesDirectoryExist listOfFilesAndDirectories
          print $ "Current directory = " ++ show currentDir
          print $ "List of files and directories = " ++ show listOfFilesAndDirectories
          print $ "List of directories = " ++ show justDirectories
          print "We are done!"



updateRepository :: FilePath -> IO ()
updateRepository path = do
    let repoOpts = RepositoryOptions { repoPath = path
                                     , repoWorkingDir = Nothing
                                     , repoIsBare = False
                                     , repoAutoCreate = False }
    repo <- openRepository repoOpts False
    return

listGitRepositories :: FilePath -> IO [FilePath]
listGitRepositories path = do directories <- listDirectory path
                              let gitRepositories = filter isGitRepository directories
                              return gitRepositories

listDirectories :: FilePath -> IO [FilePath]
listDirectories path = do subFilesAndSubDirectories <- listDirectory path
                          filterM doesDirectoryExist subFilesAndSubDirectories

isGitRepository :: FilePath -> Bool
isGitRepository ".git" = True
isGitRepository _      = False
