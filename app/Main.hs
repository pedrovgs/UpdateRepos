module Main where

import           Control.Monad
import qualified Control.Monad.Parallel as P
import           Data.Either
import           Data.List
import           Data.List.Split
import           Git
import           System.Directory
import           System.FilePath.Posix
import           UpdateRepos

main :: IO ()
main = do isReady <- isEnvironmentReady
          if not isReady then putStrLn "You need to install Git before to execute update-repos"
          else do putStrLn "Let's update your Git repositories!\n"
                  currentDir <- getCurrentDirectory
                  repositories <- listGitRepositories currentDir
                  updateReposResult <- P.mapM updateGitRepository repositories
                  putStrLn (prettyfiResults updateReposResult ++ "\n")
                  putStrLn "We are done!"

prettyfiResults :: [Either UpdateRepoError UpdateRepoSuccess] -> String
prettyfiResults results = intercalate "\n" prettyResults
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
