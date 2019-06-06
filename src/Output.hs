-- | This module gathers all functions that output something during runtime
module Output
  ( getResourceString
  ) where

import Project

import System.FilePath

getResourceString :: FilePath -> IO String
getResourceString path
  -- dataDir <- deckerResourceDir
 = do
  let dataDir = "/Users/armin/work/decker_related/resourcetest/resource/"
  readFile (dataDir </> path)
