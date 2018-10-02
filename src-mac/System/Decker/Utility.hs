{-- Author: Jan-Philipp Stauffert <jan-philipp.stauffert@uni-wuerzburg.de.de> --}

module System.Decker.Utility
    (deckerResourceDir,
    fileLink
    ) where
import System.Environment
import System.Directory
import Common
import System.FilePath

deckerResourceDir :: IO FilePath
deckerResourceDir = getXdgDirectory XdgData ("decker" ++ "-" ++  deckerVersion)

fileLink :: FilePath -> FilePath -> IO ()
fileLink target link = createFileLink target link
