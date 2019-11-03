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

import System.Directory
import qualified Data.Text as T
import qualified Data.Text.IO as TI
import Data.XML.Types
import Development.Shake

import System.FilePath
import Control.Exception
import Control.Monad.Extra
import Control.Monad.IO.Class
import Data.Maybe

import Data.Yaml
import Text.Decker.Project.Project

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
    --     course <- TI.readfile mdFile
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

-- List contents of dir (takes path of public directory), returns File tags
getFiles :: [FilePath] -> [Node]
getFiles pubFiles = map buildFileTags pubFiles             -- returns IO [FilePath]
        where
            buildFileTags file = NodeElement $ Element "file" [("href", [ContentText $ T.pack file])] []
    
buildMetadata :: Node 
buildMetadata = NodeElement $ Element "metadata" [] [schema, schemaVersion]
    where
        schema = NodeElement $ Element "schema" [] [NodeContent $ ContentText $ T.pack "ADL SCORM"]
        schemaVersion = NodeElement $ Element "schemaversion" [] [NodeContent $ ContentText $ T.pack "1.2"]

orgs :: Node
orgs = NodeElement $ Element "organizations" attrs [title, item]
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

buildResources :: FilePath -> IO Node
buildResources pubDir = do 
    pubFiles <- listDirectory pubDir 
    html <- getHtml pubFiles
    let href = html
    let resAttrs =  [("identifier", [ContentText "course-identifier_res"])
                    ,("type", [ContentText "webcontent"])
                    ,("href", [ContentText $ T.pack href])
                    ,("adlcp:scormtype", [ContentText "sco"])]
    let resChildren = getFiles pubFiles   
    let children = NodeElement $ Element "resource" resAttrs resChildren           
    return $ NodeElement $ Element "resources" [] [children]
    where 
        getHtml pubFiles = do
            file <- findFile pubFiles "deck.html$" 
            case file of 
                Just file -> return file
                Nothing -> return ""

writeManifest :: FilePath -> FilePath -> IO ()
writeManifest projDir pubDir = do
    -- get courseInfo from markdown YAML
    -- course <- getCourseInfo projDir    

    resourceTag <- buildResources pubDir
    let resource = resourceTag
    let metaData = buildMetadata
    let attrs = [ ("identifier", [ContentText "course-identifier"])
                , ("version", [ContentText "course-version"])]
    let root = Element "manifest" attrs [metaData, orgs, resource]

    -- build and write Manifest file
    let manifestDoc = T.pack $ show $ Document (Prologue [] Nothing []) root []
    let maniDir = pubDir </> "imsmanifest.xml"
    TI.writeFile maniDir manifestDoc

buildScorm :: FilePath -> FilePath -> Action ()
buildScorm proj pub = runAfter $ writeManifest proj pub