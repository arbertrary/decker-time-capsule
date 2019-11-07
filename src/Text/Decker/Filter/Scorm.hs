{-# LANGUAGE OverloadedStrings #-}
{-| 
  Module: Scorm
  Description: Provides functionality for creating different types of quiz questions
  Author: Samantha Monty <samantha.monty@uni-wuerzburg.de>
  
  This module creates an imsmanifest.xml file to enable SCORM standards used in
    Learning Management Systems.
-}
module Text.Decker.Filter.Scorm
( buildScorm
) where

import Control.Exception (throw)
import Control.Monad.Extra (partitionM)
import qualified Data.ByteString.Lazy as Lazy (writeFile)
import Data.List
import qualified Data.Text as T hiding (isSuffixOf)
import Data.Time.Clock (getCurrentTime, utctDay)
import Data.XML.Types
import Data.Yaml
import Development.Shake hiding (doesDirectoryExist)
import System.Directory
import System.FilePath
import Text.Decker.Internal.Common (pandocReaderOpts)
-- import Text.Decker.Project.Project
import Text.Pandoc.Class (runPure)
import Text.Pandoc.Definition
import Text.Pandoc.Error
import Text.Pandoc.Readers (readMarkdown)
import Text.Pandoc.Shared (stringify)
import Text.XML.Unresolved (renderLBS, def)

-- Defined in the YAML header
data Course = Course 
    { name :: String
    , identifier :: String
    , version :: String
    } deriving Show
    
getFile :: String -> [FilePath] -> FilePath
getFile target (file:fileList) = 
    case target `isSuffixOf` file of 
        True -> file
        False -> getFile target fileList
getFile target [] = error $ "Can't find " ++ target 

getYaml :: String -> Meta -> IO String   
getYaml field meta = do
    case lookupMeta field meta of 
        Just (MetaInlines s) -> return $ stringify s
        Nothing -> throw $ YamlException yamlWarning
    where
        yamlWarning = "The YAML header of your markdown file requires title, author and version."
        
-- Gets course info from YAML header
getCourse :: FilePath -> IO Course
getCourse projDir = do
    names <- listDirectory projDir
    markdownFile <- readFile $ getFile "-deck.md" names
    let markdown = T.pack markdownFile
    case runPure (readMarkdown pandocReaderOpts markdown) of
        Right pandoc@(Pandoc meta _) -> do
            title <- getYaml "title" meta
            author <- getYaml "author" meta 
            version <- getYaml "version" meta 
            date <- getCurrentTime >>= return . show . utctDay
            let identifier = (take 3 title) ++ "_" ++ (take 3 author) ++ date 
            return $ Course title identifier version 
        Left errMsg -> throw $ PandocParseError (show errMsg)
  
listFiles :: FilePath -> IO [FilePath]
listFiles dir = do
    names <- listDirectory dir
    let qualifiedNames = map (dir </>) names
    (dirs, files) <- partitionM System.Directory.doesDirectoryExist qualifiedNames   -- files returns support dir
    subFiles <- mconcat $ map listFiles dirs
    let allFiles = files ++ subFiles
    let noStores = dropWhile (\x -> x == ".DS_Store") allFiles 
    return $ map takeFileName noStores
    
buildFileTags :: FilePath -> IO [Node]
buildFileTags dir = do
    allDirs <- listFiles dir
    return $ fmap wrapTag allDirs
    where
        wrapTag file = NodeElement $ Element "file" [("href", [ContentText $ T.pack file])] []

buildResources :: FilePath -> T.Text -> IO Node
buildResources pubDir courseID = do 
    pubFiles <- listDirectory pubDir
    let resAttrs =  [("identifier", [ContentText courseID])
                    ,("type", [ContentText "webcontent"])
                    ,("href", [ContentText $ T.pack $ getFile "-deck.html" pubFiles])
                    ,("adlcp:scormtype", [ContentText "sco"])]
    resChildren <- buildFileTags pubDir   
    let children = NodeElement $ Element "resource" resAttrs resChildren          
    return $ NodeElement $ Element "resources" [] [children]

metaData :: Node 
metaData = NodeElement $ Element "metadata" [] [schema, schemaVersion]
    where
        schema = NodeElement $ Element "schema" [] [NodeContent $ ContentText $ T.pack "ADL SCORM"]
        schemaVersion = NodeElement $ Element "schemaversion" [] [NodeContent $ ContentText $ T.pack "1.2"]

buildOrg :: Course -> Node
buildOrg course = NodeElement $ Element "organizations" attrs [org]
    where
        courseName = NodeContent $ ContentText $ T.pack $ name course
        resId = ContentText $ T.pack $ identifier course ++ "_RES"
        orgId = ContentText $ T.pack $ identifier course ++ "_ORG"
        itemId = ContentText $ T.pack $ identifier course ++ "_ITEM"
        attrs = [("identifier", [orgId])]
        title = NodeElement $ Element "title" [] [courseName] 
        itemAttrs = [ ("identifier", [itemId])
                    , ("isvisible", [ContentText "true"])
                    , ("identifierref", [resId])]
        itemChildren = [NodeElement $ Element "title" [] [courseName]]
        item = NodeElement $ Element "item" itemAttrs itemChildren
        org = NodeElement $ Element "organization" attrs [title, item]  

writeManifest :: FilePath -> FilePath -> IO ()
writeManifest projDir pubDir = do
    course <- getCourse projDir  
    let courseID = T.pack $ identifier course
    let courseVersion = T.pack $ version course
    let orgs = buildOrg course
    resources <- buildResources pubDir courseID
    let attrs = [ ("identifier", [ContentText $ courseID])
                , ("version", [ContentText $ courseVersion])]
    let root = Element "manifest" attrs [metaData, orgs, resources]
    let manifestDoc = renderLBS def (Document (Prologue [] Nothing []) root [])
    Lazy.writeFile (pubDir </> "imsmanifest.xml") manifestDoc

buildScorm :: FilePath -> FilePath -> Action ()
buildScorm proj pub = runAfter $ writeManifest proj pub