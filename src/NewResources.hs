{-- Author: Armin Bernstetter <bernstetter@informatik.uni-wuerzburg.de --}
-- | This module is an interface that provides transparent access to the resources
-- Depending on specification in "decker-meta.yaml" the source of the resource folder is chosen
-- Everything that is copying or linking Resource folders needs to be moved here
-- 
module NewResources
  -- * Provisioning
  ( handleResources
  ) where

import Common
import Exception
import Meta
import Project
import Shake
import System.Decker.OS

import Codec.Archive.Zip
import Control.Exception
import Control.Lens ((^.))
import Control.Monad.Extra
import Data.Map.Strict (size)
import qualified Data.Text as T
import Data.Yaml as Yaml
import Development.Shake
import Network.URI
import qualified System.Directory as Dir
import System.Environment
import System.FilePath

-- import Text.URI (mkURI)
-- import Text.URI.Lens
{- NOTE:
resources is already in meta data (read from decker-meta.yaml)
see metaA in Shake.hs
this metadata is currently only read in Decker.hs and then propagated forward
This should stay. Here in NewResources only the ResourceType Datatype should be used.
Paths come from Project.hs

- call one "main" function from Decker.hs
- must account for "example", "support" and "template"
- must account for each ResourceType
- must account for each Provisioning variant (copy, symlink etc)

- There is a difference between the provisioning that is done in the "provision..." functions
and the copying/linking that is done when "decker support" or "decker example" happens.
provisionResources happens during the processing of markdown files

- the only directory path that has to be changed is 
(directories ^. appData)
e.g.  /home/user/.local/share/decker-0.7.3-84-resource-handling-d804821
Folder contains example, support, template folders. from there on the rest can stay the same
- COULD be changed using directories & appData .~ path
- For ResourceTypes without caching the public and support dirs could be changed accordingly
- what about the template dir? where is it called from?
I don't think it is copied anywhere so if the appData directory is correct it should not matter


Strategy for different ResourceTypes:
- Decker/Default -> same as currently. unpack from executable, unpack to .local/share
- File -> unpack from local zip archive (absolute or relative path?), unpack to .local/share
- Https -> curl/wget zip archive and immediately unpack to .local/share
- Dev -> Use resource folder here in the decker repo
- Project -> Use resource folder in slide project (no copying/symlinking to public?)
- Local -> Use resource folder anywhere
-}
getResourceMeta :: Yaml.Value -> ResourceType
getResourceMeta meta =
  case metaValueAsString "resources" meta of
    Just "project" -> Project $ "." </> "resource"
    Just resources ->
      case parseURIReference resources of
        Just (URI "file:" _ path _ _) ->
          if ".zip" `isExtensionOf` path
            then File path
            else Local path
        Just (URI "https:" _ path _ _) -> Https resources
        Just (URI "project:" _ path _ _) -> Project $ path </> "resource"
        _ -> Decker
      --   (Just "dev", _) -> Dev
      --   (Just "project", _) -> Project
      --   (_, _) -> Decker
      -- where t = lookupYamlValue "type" resources
      --       src = lookupYamlValue "source" resources
    Nothing -> Decker

handleResources :: Yaml.Value -> IO ()
handleResources meta = do
  let rt = getResourceMeta meta
  print rt
  p <- testdeckerResourceDir rt
  absp <- Dir.makeAbsolute p
  pexists <- Dir.doesDirectoryExist absp
  print absp
  print pexists
  cont <- Dir.getDirectoryContents absp
  print cont
