{-- Author: Henrik Tramberend <henrik@tramberend.de> --}
module Meta
  ( toPandocMeta
  , toMustacheMeta
  , mergePandocMeta
  , joinMeta
  , readMetaData
  , aggregateMetaData
  , metaValueAsString
  , lookupYamlValue
  , DeckerException(..)
  ) where

import Common
import Exception
import Markdown

import Control.Arrow
import Control.Exception
import qualified Data.HashMap.Strict as H

-- import qualified Data.HashMap.Lazy as H
import Data.List.Split (splitOn)
import qualified Data.Map.Lazy as Map
import qualified Data.Text as T
import qualified Data.Vector as Vec
import qualified Data.Yaml as Y
import System.FilePath
import System.FilePath.Glob
import qualified Text.Mustache.Types as MT
import Text.Pandoc

joinMeta :: Y.Value -> Y.Value -> Y.Value
joinMeta (Y.Object old) (Y.Object new) = Y.Object (H.union new old)
joinMeta (Y.Object old) _ = Y.Object old
joinMeta _ (Y.Object new) = Y.Object new
joinMeta _ _ = throw $ YamlException "Can only join YAML objects."

-- | Converts pandoc meta data to mustache meta data. Inlines and blocks are
-- rendered to markdown strings with default options.
toMustacheMeta :: MetaValue -> MT.Value
toMustacheMeta (MetaMap mmap) =
  MT.Object $ H.fromList $ map (T.pack *** toMustacheMeta) $ Map.toList mmap
toMustacheMeta (MetaList a) = MT.Array $ Vec.fromList $ map toMustacheMeta a
toMustacheMeta (MetaBool bool) = MT.Bool bool
toMustacheMeta (MetaString string) = MT.String $ T.pack string
toMustacheMeta (MetaInlines inlines) =
  MT.String $ writeMarkdownText def (Pandoc (Meta Map.empty) [Plain inlines])
toMustacheMeta (MetaBlocks blocks) =
  MT.String $ writeMarkdownText def (Pandoc (Meta Map.empty) blocks)

writeMarkdownText :: WriterOptions -> Pandoc -> T.Text
writeMarkdownText options pandoc =
  case runPure $ Markdown.writeMarkdown options pandoc of
    Right text -> text
    Left err -> throw $ PandocException $ show err

mergePandocMeta :: MetaValue -> MetaValue -> MetaValue
mergePandocMeta (MetaMap meta1) (MetaMap meta2) =
  MetaMap $ Map.union meta1 meta2
mergePandocMeta meta1 _ = meta1

-- | Converts YAML meta data to pandoc meta data.
toPandocMeta :: Y.Value -> MetaValue
toPandocMeta (Y.Object m) =
  MetaMap $ Map.fromList $ map (T.unpack *** toPandocMeta) $ H.toList m
toPandocMeta (Y.Array vector) = MetaList $ map toPandocMeta $ Vec.toList vector
toPandocMeta (Y.String text) = MetaString $ T.unpack text
toPandocMeta (Y.Number scientific) = MetaString $ show scientific
toPandocMeta (Y.Bool bool) = MetaBool bool
toPandocMeta Y.Null = MetaList []

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

lookupYamlValue :: String -> Y.Value -> Maybe Y.Value
lookupYamlValue key (Y.Object hashTable) = H.lookup (T.pack key) hashTable
lookupYamlValue _ _ = Nothing

-- | Return String of a value from yaml file
metaValueAsString :: String -> Y.Value -> Maybe String
metaValueAsString key meta =
  case splitOn "." key of
    [] -> Nothing
    k:ks -> lookup' ks (lookupYamlValue k meta)
  where
    lookup' :: [String] -> Maybe Y.Value -> Maybe String
    lookup' [] (Just (Y.String s)) = Just (T.unpack s)
    lookup' [] (Just (Y.Number n)) = Just (show n)
    lookup' [] (Just (Y.Bool b)) = Just (show b)
    lookup' (k:ks) (Just obj@(Y.Object _)) = lookup' ks (lookupYamlValue k obj)
    lookup' _ _ = Nothing
