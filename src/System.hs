{-# LANGUAGE ImplicitParams #-}

module System (listDirectoriesRecursive, separator) where

import           Control.Monad
import qualified Control.Monad.Parallel as P
import           Data.List
import           System.FilePath.Posix
import           SystemFree

type FilesPredicate = [FilePath] -> Bool
type StopSearchingPredicate = [FilePath] -> Bool

listDirectoriesRecursive :: (?boolInterpreter :: SystemFreeInterpreter Bool) => FilesPredicate -> StopSearchingPredicate -> FilePath -> IO [FilePath]
listDirectoriesRecursive predicate stopSearchingPredicate absPath =
  do let path = appendSeparatorIfNeeded absPath
     subDirectories <- listDirectories path
     let predicateMatch = predicate subDirectories
     let stopSearchingMatch = stopSearchingPredicate subDirectories
     if null subDirectories then return []
     else if predicateMatch then return [path]
     else if stopSearchingMatch then return []
     else do restOfDirectories <- P.mapM (listDirectoriesRecursive predicate stopSearchingPredicate) subDirectories
             if predicateMatch then do let restOfGitDirectories = concat restOfDirectories
                                       return (path : restOfGitDirectories)
             else return (concat restOfDirectories)

listDirectories :: (?boolInterpreter :: SystemFreeInterpreter Bool) => FilePath -> IO [FilePath]
listDirectories path = do subFilesAndSubDirectories <- run $ listDirectory' path
                          let absolutePath = map (\sub -> path ++ sub) subFilesAndSubDirectories
                              absPathsWithSeparators = map appendSeparatorIfNeeded absolutePath
                          filterM isDirectory absPathsWithSeparators

isDirectory :: (?boolInterpreter :: SystemFreeInterpreter Bool) => FilePath -> IO Bool
isDirectory absolutePath = do let isHidden = "." `isPrefixOf` absolutePath
                              isDirectory <- ?boolInterpreter $ isDirectory' absolutePath
                              return (isDirectory && not isHidden)

appendSeparatorIfNeeded :: String -> String
appendSeparatorIfNeeded absPath
  | last absPath == pathSeparator = absPath
  | otherwise = absPath ++ separator

separator = [pathSeparator]
