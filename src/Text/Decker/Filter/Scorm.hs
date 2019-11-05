{-# LANGUAGE OverloadedStrings #-}
{-| 
  Module: Scorm
  Description: Provides functionality for creating different types of quiz questions
  Author: Samantha Monty <samantha.monty@uni-wuerzburg.de>
  
  This module creates an imsmanifest.xml file to enable SCORM standards used in
    Learning Management Systems.
-}
module Text.Decker.Filter.Scorm
( buildScorm
) where

import Control.Monad (filterM)
import Control.Monad.Extra (partitionM)
import qualified Data.ByteString.Lazy as Lazy (putStr, writeFile)
import Data.List
import qualified Data.Text as T hiding (isSuffixOf)
-- import qualified Text.XML.Unresolved as XR
import Data.XML.Types
import Development.Shake hiding (doesDirectoryExist)
import System.Directory
import System.FilePath
import Text.XML.Unresolved (renderLBS, def)

-- import Data.Yaml
import Text.Decker.Project.Project
-- import Text.XML.Generator

-- Defined in the YAML header
data Course = Course 
    { name :: String
    , identifier :: String
    , version :: String
    , lessonTitle :: String
    } deriving Show


    -- nameOfCourse = name <$> Course

-- Search Project dir for MD file
    -- read YMAL to Course data
    -- getCourseInfo :: FilePath -> IO Course
    -- getCourseInfo projDir = do
    --     let mdFile = 
    --     course <- XR.readfile mdFile
    --     return $ course

    -- Gets course info from YAML header, need path of markdown file
    -- getCourse :: FilePath -> Pandoc -> Course
    -- getCourse mdFile = do
    --     markdown <- T.readFile mdFile
    --     let Pandoc metadata content = case runPure (readMarkdown _ markdown) of
    --             Right pandoc -> pandoc
    --             Left errMsg -> throw $ PandocException (show errMsg)
    --     let lessonName = maybe [] (map text) $ getField "title" metadata
    --     let courseName = maybe [] (map text) $ getField "course-name" metadata
    --     let courseId = maybe [] (map text) $ getField "course-id" metadata
    --     let courseVersion = maybe [] (map text) $ getField "course-version" metadata
    --     return $ Course courseName courseId courseVersion lessonName
  
metaData :: Node 
metaData = NodeElement $ Element "metadata" [] [schema, schemaVersion]
    where
        schema = NodeElement $ Element "schema" [] [NodeContent $ ContentText $ T.pack "ADL SCORM"]
        schemaVersion = NodeElement $ Element "schemaversion" [] [NodeContent $ ContentText $ T.pack "1.2"]

        -- <organization identifier="GOL_23.10.19_ORG">

orgs :: Node
orgs = NodeElement $ Element "organizations" attrs [org]
    where
        id = "course-identifier" ++ "_ORG"
        attrs = [("identifier", [ContentText $ T.pack id])]
        title = NodeElement $ Element "title" [] [NodeContent $ ContentText $ T.pack "course-name"] 
        itemId = "course-identifier" ++ "_item"
        itemIdRef = "course-identifier" ++ "_res"
        itemAttrs = [ ("identifier", [ContentText $ T.pack itemId])
                    , ("isvisible", [ContentText "true"])
                    , ("identifierref", [ContentText $ T.pack itemIdRef])]
        itemChildren = [NodeElement $ Element "title" [] [NodeContent $ ContentText $ T.pack "lesson-title"]]
        item = NodeElement $ Element "item" itemAttrs itemChildren
        org = NodeElement $ Element "organization" attrs [title, item]

getHtml :: [FilePath] -> FilePath
getHtml (file:fileList) = do
    let isHtml = "-deck.html" `isSuffixOf` file
    case isHtml of 
        True -> file
        False -> getHtml fileList
getHtml [] = error $ "Cant find *-deck.html" 

-- lists all directories in a directory, returns a list of all directories
listDirs :: FilePath -> IO [FilePath]
listDirs dir = do
    names <- listDirectory dir
    (dirs, files) <- partitionM doesDirectoryExist names
    return files
    -- listDirectory dir >>= filterM (doesDirectoryExist . (++) dir )

-- Expected type is return type in signature
buildFileTags :: FilePath -> IO [Node]
buildFileTags dir = do
    allDirs <- listDirs dir
    return $ fmap wrapTag allDirs
    where
        wrapTag file = NodeElement $ Element "file" [("href", [ContentText $ T.pack file])] []

buildResources :: FilePath -> IO Node
buildResources pubDir = do 
    pubFiles <- listDirectory pubDir
    let resAttrs =  [("identifier", [ContentText "course-identifier_res"])
                    ,("type", [ContentText "webcontent"])
                    ,("href", [ContentText $ T.pack $ getHtml pubFiles])
                    ,("adlcp:scormtype", [ContentText "sco"])]
    resChildren <- buildFileTags pubDir   
    let children = NodeElement $ Element "resource" resAttrs resChildren          
    return $ NodeElement $ Element "resources" [] [children]

writeManifest :: FilePath -> FilePath -> IO ()
writeManifest projDir pubDir = do
    -- get courseInfo from markdown YAML
    -- course <- getCourseInfo projDir  
    resources <- buildResources pubDir
    let attrs = [ ("identifier", [ContentText "course-identifier"])
                , ("version", [ContentText "course-version"])]
    let root = Element "manifest" attrs [metaData, orgs, resources]
    let manifestDoc = renderLBS def (Document (Prologue [] Nothing []) root [])
    Lazy.writeFile (pubDir </> "imsmanifest.xml") manifestDoc

buildScorm :: FilePath -> FilePath -> Action ()
buildScorm proj pub = runAfter $ writeManifest proj pub