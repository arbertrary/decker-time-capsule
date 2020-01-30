{-# LANGUAGE OverloadedStrings #-}

{-| 
  Module: Scorm
  Description: Provides functionality for creating imsmanifest file
  Author: Samantha Monty <samantha.monty@uni-wuerzburg.de>
  
  This module creates an imsmanifest.xml file to enable SCORM standards used in
    Learning Management Systems. This is based on SCORM 1.2.
-}
module Text.Decker.Filter.Scorm
  ( addInstructions
  , createScormPackage
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
import Text.Decker.Internal.Meta (getMetaInt, getMetaString, readMetaData)
import Text.Pandoc
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
  date <- Data.Time.Clock.getCurrentTime >>= return . show . utctDay
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

-- Add a single slide with quiz instructions to the front of the slide deck
addInstructions :: Pandoc -> Meta -> Pandoc
addInstructions pandoc@(Pandoc meta blocks) metadata =
  case getMetaString "title" meta of
    Just "Generated Index" -> pandoc
    _ -> Pandoc meta (instructionSlide ++ pointsSlide ++ blocks)
  where
    instructionSlide =
      [ Header 1 ("instructionsSlide", [], []) [Str "Instructions"]
      , Para [Str read]
      , BulletList
          [ [Para [Str understand]]
          , [Para [Str mc]]
          , [Para (Str points : pointSpan)]
          ]
      ]
    read =
      "Carefully read and understand the following directions before you start."
    understand =
      "Be sure to read the exercises carefully. First understand, then solve!"
    mc = "Multiple choice questions may have several correct answers."
    points = "The maximum number of points to achieve is "
    pointSpan =
      [ RawInline
          "html"
          "<span id =\"maxPoints\" style=\"font-weight:bold;\"></span>."
      ]
    pointsSlide =
      [ Header 1 ("pointsSlide", [], []) [Str "Points Calculation"]
      , Para
          (Str pointsCalc : Strong [Emph [Str " c/a * max"]] : [Str " where:"])
      , BulletList
          [ [Para (Emph [Str "max "] : [Str max])]
          , [Para (Emph [Str "a "] : [Str a])]
          , [Para (Emph [Str "c "] : [Str c])]
          ]
      , Para [Str select]
      ]
    pointsCalc = "The points per exercise are calculated as follows:"
    max = "is the maximum number of points for that exercise."
    a =
      "is the overall number of possible alternative answers provided for that exercise."
    c =
      "is the number of correct answers given minus the number of incorrect answers given. Correct answers are all answers which are selected and correct and those left unselected and not correct."
    select =
      "Selecting none or all boxes of an exercise will result in a score of 0 points for that exercise."
