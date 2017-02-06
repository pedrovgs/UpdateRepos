module Main where

import           Control.Monad
import           Data.Either
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
                  updateReposResult <- mapM updateGitRepository repositories
                  putStrLn (prettyfiResults updateReposResult)
                  putStrLn "We are done!"

prettyfiResults :: [Either UpdateRepoError UpdateRepoSuccess] -> String
prettyfiResults results = foldr (++) "\n" prettyResults
                          where prettyResults = map prettifyResult results

prettifyResult :: Either UpdateRepoError UpdateRepoSuccess -> String
prettifyResult (Right (UpdateRepoSuccess path result)) = "SUCCESS!! Repo: " ++ prettifyPath path ++ " updated with result " ++ prettifyStdOut result
prettifyResult (Left (UpdateRepoError path result))    = "ERROR :_( Repo: " ++ prettifyPath path ++ " updated with result " ++ prettifyStdOut result

prettifyPath :: FilePath -> String
prettifyPath path = last parts
                    where parts = splitOn [pathSeparator] path

prettifyStdOut :: String -> String
prettifyStdOut = filter (/= '\n')
