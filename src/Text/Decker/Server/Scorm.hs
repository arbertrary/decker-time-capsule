module Text.Decker.Server
  ( buildManifest
  , listResourceDir
  ) where

import qualified System.Directory as Dir
import qualified Data.Yaml as Y

let xmlTag = "<?xml version=\"1.0\" standalone=\"no\" ?>"
let manifestOpenTag = "<manifest identifier \"" 
    ++ course-name CourseInfo 
    ++ "\" version=\""
    ++ course-version CourseInfo
    ++ "\">"
let metadataTag = "<metadata><schema>ADL SCORM</schema><schemaversion>1.2</schemaversion></metadata>"
let orgsTag = 

let orgTag = 

let titleTag = 

let itemTag = 



let resourcesTag =

let resourceTag = 

let fileTag = 

-- Defined in the YAML header
data CourseInfo = CourseInfo {
    course-name :: String,
    course-ID :: String,
    course-version :: Double }
    deriving (Show)

instance FromJSON CourseInfo where
    parseJSON (Object v) = CourseInfo <$>
                            v .: "course-name" <*>
                            v .: "course-ID" <*>
                            v .: "course-version"
    parseJSON _ = error "Can't parse course information from YAML header"


-- first need to build xml file
buildManifest :: FilePath -> IO [FilePath]
buildManifest fp = do
    whenM (Dir.doesDirectoryExist fp) $
        throw (userError "Manifest file already exists.")
    handle <- openFile "imsmanifest.xml" ReadWriteMode
    handle.hPutStrLn xmlTag
    handle.hPutStrLn manifestOpenTag

    -- hPrint :: Show a -> Handle -> a -> IO ()
    -- Computation hPrint hdl t writes the string representation of t given by the shows function to the file or channel managed by hdl and appends a newline.
    -- This operation may fail with:
    --    isFullError if the device is full; or
    --    isPermissionError if another system resource limit would be exceeded.
    handle.hPrint manifestFile

-- List contents of dir (takes path of dir and path of manifest)
listResourceDir :: FilePath -> FilePath -> IO [FilePath]
listResourceDir fp mnfst = do
    unlessM (Dir.doesDirectoryExist fp) $
        throw (userError "Error when compiling resources for manifest.")
    mnfstExists <- Dir.doesFileExist mnfst
    if mnfstExists
        then 
            files <- Dir.listDirectory fp
            map buildFileTag files   
        where
            buildFileTag fi = "<file href=/" ++ fi ++ "/"/>"

            -- {-# LANGUAGE OverloadedStrings #-}
            -- import Data.Yaml
            -- import Control.Applicative -- <$>, <*>
            -- import Data.Maybe (fromJust)
            
            -- import qualified Data.ByteString.Char8 as BS
            
            -- data MyUser = MyUser {id :: Int,
            --                       name :: String,
            --                       reputation :: Int}
            --                       deriving (Show)
            
            -- instance FromJSON MyUser where
            --     parseJSON (Object v) = MyUser <$>
            --                            v .: "id" <*>
            --                            v .: "name" <*>
            --                            v .: "reputation"
            --     -- A non-Object value is of the wrong type, so fail.
            --     parseJSON _ = error "Can't parse MyUser from YAML/JSON"
            
            -- main = do
            --          ymlData <- BS.readFile "users.yml"
            --          let users = Data.Yaml.decode ymlData :: Maybe [MyUser]
            --          -- Print it, just for show
            --          print $ fromJust users