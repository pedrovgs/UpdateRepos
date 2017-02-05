module Main where

import           Control.Monad
import           System.Directory
import           UpdateRepos

main :: IO ()
main = do isReady <- isEnvironmentReady
          if not isReady then print "You need to install Git before to execute update-repos"
          else do print "Let's update your Git repositories!"
                  currentDir <- getCurrentDirectory
                  gitDirectories <- listGitRepositories currentDir
                  print $ "Current directory = " ++ show currentDir
                  print $ "List of GIT directories = " ++ show gitDirectories
                  print "We are done!"
