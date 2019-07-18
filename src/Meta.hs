{-- Author: Henrik Tramberend <henrik@tramberend.de> --}
module Meta
  ( toPandocMeta
  , toMustacheMeta
  , mergePandocMeta
  , joinMeta
  , readMetaData
  , aggregateMetaData
  , lookupMetaBool
  , lookupMetaString
  , lookupMetaInt
  , DeckerException(..)
  ) where

import Common
import Exception
import Markdown

import Control.Arrow
import Control.Exception
import qualified Data.HashMap.Strict as H
import Data.List.Safe ((!!))
import Data.List.Split
import qualified Data.Map.Lazy as Map
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Vector as Vec
import qualified Data.Yaml as Y
import Debug.Trace
import Prelude hiding ((!!))
import System.FilePath
import System.FilePath.Glob
import qualified Text.Mustache.Types as MT
import Text.Pandoc
import Text.Pandoc.Shared
import Text.Read
import Text.Regex.TDFA

joinMeta :: Y.Value -> Y.Value -> Y.Value
joinMeta (Y.Object old) (Y.Object new) = Y.Object (H.union new old)
joinMeta (Y.Object old) _ = Y.Object old
joinMeta _ (Y.Object new) = Y.Object new
joinMeta _ _ = throw $ YamlException "Can only join YAML objects."

-- | Converts pandoc meta data to mustache meta data. Inlines and blocks are
-- rendered to markdown strings with default options.
toMustacheMeta :: Meta -> MT.Value
toMustacheMeta (Meta map) = toMustacheMeta' (MetaMap map)

toMustacheMeta' :: MetaValue -> MT.Value
toMustacheMeta' (MetaMap mmap) =
  MT.Object $ H.fromList $ map (T.pack *** toMustacheMeta') $ Map.toList mmap
toMustacheMeta' (MetaList a) = MT.Array $ Vec.fromList $ map toMustacheMeta' a
toMustacheMeta' (MetaBool bool) = MT.Bool bool
toMustacheMeta' (MetaString string) = MT.String $ T.pack string
toMustacheMeta' (MetaInlines inlines) =
  MT.String $ writeMarkdownText def (Pandoc (Meta Map.empty) [Plain inlines])
toMustacheMeta' (MetaBlocks blocks) =
  MT.String $ writeMarkdownText def (Pandoc (Meta Map.empty) blocks)

writeMarkdownText :: WriterOptions -> Pandoc -> T.Text
writeMarkdownText options pandoc =
  case runPure $ Markdown.writeMarkdown options pandoc of
    Right text -> text
    Left err -> throw $ PandocException $ show err

mergePandocMeta :: Meta -> Meta -> Meta
mergePandocMeta (Meta map1) (Meta map2) = Meta $ Map.union map1 map2

mergePandocMeta' :: MetaValue -> MetaValue -> MetaValue
mergePandocMeta' (MetaMap meta1) (MetaMap meta2) =
  MetaMap $ Map.union meta1 meta2
mergePandocMeta' meta1 _ = meta1

-- | Converts YAML meta data to pandoc meta data.
toPandocMeta :: Y.Value -> Meta
toPandocMeta v@(Y.Object m) = 
  case toPandocMeta' v of
    (MetaMap map) -> Meta map
    _ -> Meta M.empty
toPandocMeta _ = Meta M.empty 
  
toPandocMeta' :: Y.Value -> MetaValue
toPandocMeta' (Y.Object m) =
  MetaMap $ Map.fromList $ map (T.unpack *** toPandocMeta') $ H.toList m
toPandocMeta' (Y.Array vector) = MetaList $ map toPandocMeta' $ Vec.toList vector
toPandocMeta' (Y.String text) = MetaString $ T.unpack text
toPandocMeta' (Y.Number scientific) = MetaString $ show scientific
toPandocMeta' (Y.Bool bool) = MetaBool bool
toPandocMeta' Y.Null = MetaList []

decodeYaml :: FilePath -> IO Y.Value
decodeYaml yamlFile = do
  result <- Y.decodeFileEither yamlFile
  case result of
    Right object@(Y.Object _) -> return object
    Right _ ->
      throw $
      Exception.YamlException $
      "Top-level meta value must be an object: " ++ yamlFile
    Left exception -> throw exception

readMetaData :: FilePath -> IO Y.Value
readMetaData dir = do
  files <- globDir1 (compile "*-meta.yaml") dir
  meta <- mapM decodeYaml files
  return $ foldl joinMeta (Y.object []) meta

aggregateMetaData :: FilePath -> FilePath -> IO Y.Value
aggregateMetaData top = walkUpTo
  where
    walkUpTo dir = do
      fromHere <- readMetaData dir
      if equalFilePath top dir
        then return fromHere
        else do
          fromAbove <- walkUpTo (takeDirectory dir)
          return $ joinMeta fromHere fromAbove

-- | Split a compound meta key at the dots and separate the array indexes.
splitKey =
  concatMap (split (keepDelimsL (Data.List.Split.oneOf "["))) . splitOn "."

-- | Extract the bracketed array index string.
arrayIndex :: String -> Maybe String
arrayIndex key =
  listToMaybe $
  reverse (getAllTextSubmatches (key =~ ("^\\[([0-9]+)\\]$" :: String)))

-- | Recursively deconstract a compound key and drill into the meta data hierarchy.
lookupMeta' :: Meta -> String -> Maybe MetaValue
lookupMeta' meta key = lookup' (splitKey key) (MetaMap (unMeta meta))
  where
    lookup' (key:path) (MetaMap map) = M.lookup key map >>= lookup' path
    lookup' (key:path) (MetaList list) =
      arrayIndex key >>= readMaybe >>= (!!) list >>= lookup' path
    lookup' [] mv = Just mv

-- | Lookup a boolean value in a Pandoc meta data hierarchy. The key string
-- notation is indexed subkeys separated by '.', eg. `top.list[3].value`.
lookupMetaBool :: Meta -> String -> Maybe Bool
lookupMetaBool meta key = lookupMeta' meta key >>= metaToBool

metaToBool :: MetaValue -> Maybe Bool
metaToBool (MetaBool bool) = Just bool
metaToBool _ = Nothing

-- | Lookup a String value in a Pandoc meta data hierarchy. The key string
-- notation is indexed subkeys separated by '.', eg. `top.list[3].value`.
lookupMetaString :: Meta -> String -> Maybe String
lookupMetaString meta key = lookupMeta' meta key >>= metaToString

metaToString :: MetaValue -> Maybe String
metaToString (MetaString string) = Just string
metaToString (MetaInlines inlines) = Just $ stringify inlines
metaToString _ = Nothing

lookupMetaInt :: Meta -> String -> Maybe Int
lookupMetaInt meta key = lookupMetaString meta key >>= readMaybe
