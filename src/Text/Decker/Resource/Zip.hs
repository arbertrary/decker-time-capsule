module Text.Decker.Resource.Zip
  ( extractResourceEntries
  , extractResourceEntry
  , extractResourceEntryList
  , zipDirectory
  ) where

import Text.Decker.Internal.Exception
import Text.Decker.Project.Project

import Codec.Archive.Zip
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString as BS
import Data.List (isPrefixOf)
import Data.Map.Strict (filterWithKey, keys, size)
import qualified System.Directory as Dir
import System.Environment
import System.FilePath

-- | Extracts entries from the embedded resource archive that match the prefix
-- directory into the destination directory. The entry path from the archive is
-- preserverd and non-existent intermediate directories are created. Existing
-- files are overwritten.
extractResourceEntries :: FilePath -> FilePath -> IO ()
extractResourceEntries prefix destinationDirectory = do
  deckerExecutable <- getExecutablePath
  withArchive deckerExecutable $ do
    subEntries <- filterWithKey (subEntry prefix) <$> getEntries
    forM_ (keys subEntries) saveSubEntry
  where
    subEntry dir sel _ = dir `isPrefixOf` unEntrySelector sel
    saveSubEntry sel = do
      let path = destinationDirectory </> unEntrySelector sel
      let dir = takeDirectory path
      liftIO $ Dir.createDirectoryIfMissing True dir
      saveEntry sel path

extractResourceEntry :: FilePath -> IO BS.ByteString
extractResourceEntry entryName = do
  deckerExecutable <- getExecutablePath
  withArchive deckerExecutable $ mkEntrySelector entryName >>= getEntry

extractResourceEntryList :: [FilePath] -> IO [(FilePath, BS.ByteString)]
extractResourceEntryList entryNames = do
  deckerExecutable <- getExecutablePath
  withArchive deckerExecutable $ foldM extractEntry [] entryNames
  where
    extractEntry entryList entryName = do
      bs <- mkEntrySelector entryName >>= getEntry
      return $ (entryName, bs) : entryList

-- | Extract resources from the executable into the XDG data directory.
extractResources :: IO ()
extractResources = do
  deckerExecutable <- getExecutablePath
  dataDir <- deckerResourceDir
  exists <- Dir.doesDirectoryExist dataDir
  unless exists $ do
    numFiles <- withArchive deckerExecutable getEntries
    unless ((size numFiles) > 0) $
      throw $ ResourceException "No resource zip found in decker executable."
    Dir.createDirectoryIfMissing True dataDir
    withArchive deckerExecutable (unpackInto dataDir)
    putStrLn $ "# resources extracted to " ++ dataDir

zipDirectory :: FilePath -> FilePath -> IO ()
zipDirectory src out = do
  let archive = packDirRecur Deflate mkEntrySelector src
  createArchive out archive
