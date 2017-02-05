module Main where

import           Control.Monad
import           System.Directory
import           UpdateRepos

main :: IO ()
main = do print "Let's update your Git repositories!"
          currentDir <- getCurrentDirectory
          gitDirectories <- listGitRepositories currentDir
          print $ "Current directory = " ++ show currentDir
          print $ "List of GIT directories = " ++ show gitDirectories
          print "We are done!"
