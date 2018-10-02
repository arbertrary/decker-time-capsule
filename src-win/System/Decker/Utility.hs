{-- Author: Jan-Philipp Stauffert <jan-philipp.stauffert@uni-wuerzburg.de.de> --}

module System.Decker.Utility
    (deckerResourceDir,
    fileLink
    ) where
-- import Utilities as U
import System.Environment
import System.Directory
import Common
import Debug.Trace
import System.FilePath

deckerResourceDir :: IO FilePath
deckerResourceDir = do
    exep <- getExecutablePath
    return $ (trace exep exep) </> ".." </> "resources"

fileLink :: FilePath -> FilePath -> IO ()
fileLink target link = copyRecursive target link
copyRecursive :: FilePath -> FilePath -> IO ()
copyRecursive src dest = do
    isDir <- doesDirectoryExist src
    if isDir
        then do
            contents <- listDirectory src
            mapM (\x -> copyRecursive (src </> x) (dest </> x)) contents
            return ()
        else copyFileWithMetadata src dest