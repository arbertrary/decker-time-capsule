{-# LANGUAGE OverloadedStrings #-}

{-| 
  Module: Scorm
  Description: Provides functionality for creating imsmanifest file
  Author: Samantha Monty <samantha.monty@uni-wuerzburg.de>
  
  This module creates an imsmanifest.xml file to enable SCORM standards used in
    Learning Management Systems. This is based on SCORM 1.2.
-}
module Text.Decker.Filter.Scorm
  ( createScormPackage
  , getFile
  ) where

import Codec.Archive.Zip
import Control.Exception (throw)
import Control.Monad.Extra (partitionM)
import qualified Data.ByteString.Lazy as Lazy (writeFile)
import Data.List
import qualified Data.Text as T hiding (isSuffixOf)
import Data.Time.Clock (getCurrentTime, utctDay)
import Data.XML.Types
import Data.Yaml (YamlException(YamlException))
import Development.Shake hiding (doesDirectoryExist)
import qualified System.Directory as Dir
import System.FilePath
import Text.Decker.Internal.Meta (getMetaString, readMetaData)
import Text.XML.Unresolved (def, renderLBS)

-- Defined in the YAML header
data Course =
  Course
    { name :: String
    , identifier :: String
    , version :: String
    }
  deriving (Show)

getFile :: String -> [FilePath] -> FilePath
getFile target (file:fileList) =
  case target `isSuffixOf` file of
    True -> file
    False -> getFile target fileList
getFile target [] = error $ "Can't find " ++ target

-- gets values from decker.yaml
getYaml :: FilePath -> String -> IO String
getYaml dir target = do
  meta <- liftIO $ readMetaData $ dir
  case getMetaString target meta of
    Just s -> return s
    Nothing -> throw $ YamlException "The YAML value was not found."

-- Gets course info from YAML header
getCourse :: FilePath -> IO Course
getCourse projDir = do
  course <- getYaml projDir "course"
  author <- getYaml projDir "author"
  version <- getYaml projDir "version"
  date <- getCurrentTime >>= return . show . utctDay
  let identifier = (take 3 course) ++ "_" ++ (take 3 author) ++ "_" ++ date
  return $ Course course identifier version

listFiles :: FilePath -> IO [FilePath]
listFiles dir = do
  names <- Dir.listDirectory dir
  let qualifiedNames = map (dir </>) names
  (dirs, files) <- partitionM Dir.doesDirectoryExist qualifiedNames -- files returns support dir
  subFiles <- mconcat $ map listFiles dirs
  let allFiles = files ++ subFiles
  let noStores = dropWhile (\x -> x == ".DS_Store") allFiles
  return $ map takeFileName noStores

buildFileTags :: FilePath -> IO [Node]
buildFileTags dir = do
  allDirs <- listFiles dir
  return $ fmap wrapTag allDirs
  where
    wrapTag file =
      NodeElement $ Element "file" [("href", [ContentText $ T.pack file])] []

buildResources :: FilePath -> Course -> IO Node
buildResources pubDir course = do
  pubFiles <- Dir.listDirectory pubDir
  let resId = ContentText $ T.pack $ identifier course ++ "_RES"
  let href = ContentText $ T.pack $ getFile "-deck.html" pubFiles
  let resAttrs =
        [ ("identifier", [resId])
        , ("type", [ContentText "webcontent"])
        , ("href", [href])
        , ("adlcp:scormtype", [ContentText "sco"])
        ]
  resChildren <- buildFileTags pubDir
  let children = NodeElement $ Element "resource" resAttrs resChildren
  return $ NodeElement $ Element "resources" [] [children]

metaData :: Node
metaData = NodeElement $ Element "metadata" [] [schema, schemaVersion]
  where
    schema =
      NodeElement $
      Element "schema" [] [NodeContent $ ContentText $ T.pack "ADL SCORM"]
    schemaVersion =
      NodeElement $
      Element "schemaversion" [] [NodeContent $ ContentText $ T.pack "1.2"]

buildOrg :: Course -> Node
buildOrg course = NodeElement $ Element "organizations" attrs [org]
  where
    courseName = NodeContent $ ContentText $ T.pack $ name course
    resId = ContentText $ T.pack $ identifier course ++ "_RES"
    orgId = ContentText $ T.pack $ identifier course ++ "_ORG"
    itemId = ContentText $ T.pack $ identifier course ++ "_ITEM"
    attrs = [("identifier", [orgId])]
    title = NodeElement $ Element "title" [] [courseName]
    itemAttrs =
      [ ("identifier", [itemId])
      , ("isvisible", [ContentText "true"])
      , ("identifierref", [resId])
      ]
    itemChildren = [NodeElement $ Element "title" [] [courseName]]
    item = NodeElement $ Element "item" itemAttrs itemChildren
    org = NodeElement $ Element "organization" attrs [title, item]

writeManifest :: FilePath -> FilePath -> IO ()
writeManifest projDir pubDir = do
  course <- getCourse projDir
  let courseID = T.pack $ identifier course
  let courseVersion = T.pack $ version course
  let orgs = buildOrg course
  resources <- buildResources pubDir course
  let attrs =
        [ ("identifier", [ContentText $ courseID])
        , ("version", [ContentText $ courseVersion])
        ]
  let root = Element "manifest" attrs [metaData, orgs, resources]
  let manifestDoc = renderLBS def (Document (Prologue [] Nothing []) root [])
  Lazy.writeFile (pubDir </> "imsmanifest.xml") manifestDoc

createScormPackage :: FilePath -> FilePath -> Action ()
createScormPackage projDir pubDir = do
  let archPath = projDir </> "SCORM-package.zip"
  let archive = packDirRecur Deflate mkEntrySelector pubDir
  runAfter $ writeManifest projDir pubDir
  runAfter $ createArchive archPath archive
  runAfter $ putStrLn "Finished building SCORM package."
