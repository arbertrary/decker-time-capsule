{-- Author: Henrik Tramberend <henrik@tramberend.de> --}
module Common
  ( addScript
  -- * Types
  , DeckerState(..)
  , Layout(..)
  , OutputFormat(..)
  , Disposition(..)
  , MediaType(..)
  , ResourceType(..)
  , Provisioning(..)
  , Script(..)
  , Decker
  -- *
  , doIO
  , needFile
  , needFiles
  -- * Version information
  , deckerVersion
  , deckerGitBranch
  , deckerGitCommitId
  , deckerGitVersionTag
  , isDevelopmentVersion
  -- * Dealing with file suffixes 
  , dropSuffix
  , replaceSuffix
  , repeatIfTrue
  , whenTrue
  , deckSuffix
  , deckHTMLSuffix
  , deckPDFSuffix
  , pageSuffix
  , pageHTMLSuffix
  , pagePDFSuffix
  , handoutSuffix
  , handoutHTMLSuffix
  , handoutPDFSuffix
  , metaSuffix
  , indexSuffix
  , sourceSuffixes
  -- *
  , unique
  , time
  , elementAttributes
  -- * keys
  , runtimeMetaKeys
  , compiletimeMetaKeys
  , templateOverrideMetaKeys
  , metaKeys
  ) where

import CompileTime
import Control.Monad.State
import qualified Data.List.Extra as List
import Data.Maybe
import qualified Data.Set as Set
import Data.Version (showVersion, versionBranch)
import Debug.Trace
import Development.Shake (Action, need)
import Network.URI as U
import Paths_decker (version)
import System.CPUTime
import Text.Printf
import Text.Read (readMaybe)
import Text.Regex.TDFA

-- | The version from the cabal file
deckerVersion :: String
deckerVersion = showVersion version

-- | Determines the git branch at compile time 
deckerGitBranch :: String
deckerGitBranch = $(lookupGitBranch)

-- | Determines the git branch at compile time 
deckerGitCommitId :: String
deckerGitCommitId = $(lookupGitCommitId)

-- | Determines the git tag at compile time 
deckerGitVersionTag :: String
deckerGitVersionTag = $(lookupGitTag)

-- | Regex that matches a version tag
tagRegex = "v([0-9]+)[.]([0-9]+)[.]([0-9]+)" :: String

-- | Returns the tagged version as an array of strings.
deckerGitVersionTag' :: [String]
deckerGitVersionTag' =
  case getAllTextSubmatches $ deckerGitVersionTag =~ tagRegex of
    [] -> []
    m:ms -> ms

isVersionTagMatching :: Bool
isVersionTagMatching =
  versionBranch version == mapMaybe readMaybe deckerGitVersionTag'

-- | Is this a development or a production branch? Release versions are cut from
-- the master branch and carry a version tag (vX.Y.Z) that matches the version
-- entry in `package.yaml`. Everything else is a development version.
isDevelopmentVersion :: Bool
isDevelopmentVersion = not (deckerGitBranch == "master" && isVersionTagMatching)

type Decker = StateT DeckerState Action

doIO :: IO a -> Decker a
doIO = lift . liftIO

needFile :: FilePath -> Decker ()
needFile file = lift $ need [file]

needFiles :: [FilePath] -> Decker ()
needFiles pathes = lift $ need pathes

addScript :: Script -> Decker ()
addScript script = modify (\s -> s {scripts = scripts s ++ [script]})

data DeckerState = DeckerState
  { basePath :: String
  , disposition :: Disposition
  , provisioning :: Provisioning
  , slideCount :: Int
  , externalReferences :: [U.URI]
  , scripts :: [Script]
  } deriving (Eq, Show)

data Script
  = ScriptURI { scriptLang :: String
              , scriptUri :: String }
  | ScriptSource { scriptLang :: String
                 , scriptSource :: String }
  deriving (Eq, Show, Ord)

data Layout
  = Deck
  | Page
  | Handout
  deriving (Eq, Show)

data OutputFormat
  = Reveal
  | Html
  | Latex
  deriving (Eq, Show)

data Disposition = Disposition
  { layout :: Layout
  , format :: OutputFormat
  } deriving (Eq, Show)

data MediaType
  = ImageMedia
  | AudioMedia
  | VideoMedia
  | IframeMedia
  | MeshMedia

data Provisioning
  = Copy -- ^ Copy to public and relative URL
  | SymLink -- ^ Symbolic link to public and relative URL
  | Absolute -- ^ Absolute local URL
  | Relative -- ^ Relative local URL
  deriving (Eq, Show, Read)

-- | Source of the resource bundle
data ResourceType
  = Decker -- ^ decker executable, gets extracted and cached
  | File String -- ^ local ZIP archive, gets extracted and cached
  | Https String -- ^ remote ZIP archive, gets downloaded, extracted and cached
  -- | Dev -- ^ local resource folder in decker repo, no caching
  -- | Project String -- ^ local resource folder in slide project, no caching
  | Local String -- ^ local resource folder anywhere, no caching
  deriving (Eq, Show, Read)

repeatIfTrue :: Monad m => m Bool -> m ()
repeatIfTrue action = do
  again <- action
  when again $ repeatIfTrue action

whenTrue :: Monad m => m Bool -> m () -> m ()
whenTrue bool action = do
  true <- bool
  when true action

-- | Removes the last suffix from a filename
dropSuffix :: String -> String -> String
dropSuffix s t = fromMaybe t (List.stripSuffix s t)

replaceSuffix :: String -> String -> String -> String
replaceSuffix srcSuffix targetSuffix filename =
  dropSuffix srcSuffix filename ++ targetSuffix

unique :: Ord a => [a] -> [a]
unique = Set.toList . Set.fromList

time :: String -> IO a -> IO a
time name action = do
  start <- getCPUTime
  result <- action
  stop <- getCPUTime
  let diff = fromIntegral (stop - start) / (10 ^ 12)
  printf "%s: %0.5f sec\n" name (diff :: Double)
  return result

deckSuffix = "-deck.md"

deckHTMLSuffix = "-deck.html"

deckPDFSuffix = "-deck.pdf"

pageSuffix = "-page.md"

pageHTMLSuffix = "-page.html"

pagePDFSuffix = "-page.pdf"

handoutSuffix = "-deck.md"

handoutHTMLSuffix = "-handout.html"

handoutPDFSuffix = "-handout.pdf"

metaSuffix = "-meta.yaml"

indexSuffix = "-deck-index.yaml"

sourceSuffixes = [deckSuffix, pageSuffix, indexSuffix]

-- | These resources are needed at runtime. If they are specified as local URLs,
-- the resource must exists at compile time. Remote URLs are passed through
-- unchanged.
elementAttributes :: [String]
elementAttributes =
  [ "src"
  , "data-src"
  , "data-markdown"
  , "data-background-video"
  , "data-background-image"
  , "data-background-iframe"
  ]

-- | Resources in meta data that are needed at compile time. They have to be
-- specified as local URLs and must exist.
runtimeMetaKeys :: [String]
runtimeMetaKeys = ["css"]

templateOverrideMetaKeys :: [String]
templateOverrideMetaKeys = ["template"]

compiletimeMetaKeys :: [String]
compiletimeMetaKeys = ["bibliography", "csl", "citation-abbreviations"]

metaKeys :: [String]
metaKeys = runtimeMetaKeys ++ compiletimeMetaKeys ++ templateOverrideMetaKeys
