{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad
import qualified Control.Monad.Parallel as P
import           Data.Either
import           Data.List
import           Data.List.Split
import           Git
import           Options.Generic
import           System.Directory
import           System.FilePath.Posix
import           SystemFree
import           UpdateRepos
import           System.Exit
import           System

data UpdateReposParams = UpdateReposParams { path :: Maybe String } deriving (Generic, Show)

instance ParseRecord UpdateReposParams

main :: IO ()
main = do let ?systemInterpreter = run
              ?boolInterpreter = run
          params <- getRecord "update-repos command-line tool help"
          isReady <- isEnvironmentReady
          if not isReady then putStrLn "You need to install Git before to execute update-repos"
          else do currentPath <- getCurrentDirectory
                  case params of
                       UpdateReposParams (Just path) -> do let rootPath = appendSeparatorIfNeeded path
                                                           isValidDirectory <- doesDirectoryExist rootPath
                                                           if isValidDirectory then updateRepos path
                                                           else putStrLn $ "update-repos: no such file or directory:" ++ rootPath
                       UpdateReposParams Nothing     -> updateRepos currentPath


updateRepos :: (?boolInterpreter :: SystemFreeInterpreter Bool) => (?systemInterpreter :: SystemFreeInterpreter (ExitCode, String, String)) => String -> IO ()
updateRepos path = do putStrLn "Let's update your Git repositories!\n"
                      repositories <- listGitRepositories path
                      updateReposResult <- P.mapM updateGitRepository repositories
                      if null updateReposResult then putStrLn "There are no repos.\n"
                      else putStrLn (prettifyResults updateReposResult ++ "\n")
                      putStrLn "We are done!"

prettifyResults :: [Either UpdateRepoError UpdateRepoSuccess] -> String
prettifyResults results = intercalate "\n" prettyResults
                          where prettyResults = map prettifyResult results

prettifyResult :: Either UpdateRepoError UpdateRepoSuccess -> String
prettifyResult (Right (UpdateRepoSuccess path result)) = greenColor ++ "Success! Repo: " ++ prettifyPath path ++ " updated with result: " ++ prettifyStdOut result ++ clearColor
prettifyResult (Left (UpdateRepoError path result))    = redColor ++ "Error :_( Repo: " ++ prettifyPath path ++ " updated with result " ++ prettifyStdOut result  ++ clearColor

prettifyPath :: FilePath -> String
prettifyPath path = last $ init parts
                    where parts = splitOn [pathSeparator] path

prettifyStdOut :: String -> String
prettifyStdOut = filter (/= '\n')

clearColor = "\x1b[0m"
greenColor = "\x1b[32m"
redColor = "\x1b[31m"
