{-- Author: Jan-Philipp Stauffert <jan-philipp.stauffert@uni-wuerzburg.de.de> --}
module System.Decker.OS
  ( deckerResourceDir
  , fileLink
  , urlPath
  ) where

import Common
import System.Directory
import System.Environment
import System.FilePath

deckerResourceDir :: IO FilePath
deckerResourceDir = getXdgDirectory XdgData ("decker" ++ "-" ++ deckerVersion)

fileLink :: FilePath -> FilePath -> IO ()
fileLink target link = createFileLink target link

urlPath :: FilePath -> FilePath
urlPath path = path
