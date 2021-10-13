{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | This is the new Decker filter for Pandoc.
--
-- All decker specific meta data is embedded into the document meta data under
-- the `decker` key. Information gathered during the filter run is appended
-- under the `decker` key in the meta data of the resulting document.
module Text.Decker.Filter.Decker2 (runFilter2, mediaFilter2) where

import qualified Data.List as List
import Relude
import Text.Decker.Filter.Header
import Text.Decker.Filter.Local
import Text.Decker.Filter.Media
import Text.Decker.Filter.Monad
import Text.Decker.Filter.Util (oneImagePerLine)
import Text.Decker.Internal.Common
import Text.Pandoc hiding (lookupMeta)
import Text.Pandoc.Walk

-- | Applies a filter to each pair of successive elements in a list. The filter
-- may consume the elements and return a list of transformed elements, or it
-- may reject the pair and return Nothing.
pairwise :: ((a, a) -> Filter (Maybe [a])) -> [a] -> Filter [a]
pairwise f (x : y : zs) = do
  match <- f (x, y)
  case match of
    Just rs -> (rs ++) <$> pairwise f zs
    Nothing -> (x :) <$> pairwise f (y : zs)
pairwise _ xs = return xs

-- | Applies a filter to each triplet of successive elements in a list.
-- The filter may consume the elements and return a list of transformed elements,
-- or it may reject the triplet and return Nothing.
tripletwise :: ((a, a, a) -> Filter (Maybe [a])) -> [a] -> Filter [a]
tripletwise f (w : x : y : zs) = do
  match <- f (w, x, y)
  case match of
    Just rs -> (rs ++) <$> tripletwise f zs
    Nothing -> (w :) <$> tripletwise f (x : y : zs)
tripletwise _ xs = return xs

-- | Runs the document through the four increasingly detailed filter stages. The
-- matching granularity ranges from list of blocks to single inline elements.
mediaFilter2 :: Disposition -> WriterOptions -> Pandoc -> IO Pandoc
mediaFilter2 dispo options pandoc =
  runFilter2 dispo options transformHeader1 pandoc
    >>= runFilter2 dispo options mediaBlockListFilter
    >>= runFilter2 dispo options mediaInlineListFilter
    >>= runFilter2 dispo options mediaBlockFilter
    >>= runFilter2 dispo options mediaInlineFilter

-- | Filters lists of Blocks that can match in pairs or triplets.
--
-- For example: Match a paragraph containing just an image or codeblock followed
-- by a paragraph starting with the string "Caption: " as an image that is to be
-- placed in a figure block with a caption. Image or CodeBlock elements with a
-- processed class attribute are ignored.
mediaBlockListFilter :: [Block] -> Filter [Block]
mediaBlockListFilter blocks = do
  tripletwise filterTriplets blocks >>= pairwise filterPairs
  where
    filterPairs :: (Block, Block) -> Filter (Maybe [Block])
    -- An image followed by an explicit caption paragraph.
    filterPairs (Para [Image attr alt (url, title)], Para (Str "Caption:" : caption))
      | unprocessed attr =
        Just . single <$> compileImage attr alt url title caption
    -- An code block followed by an explicit caption paragraph.
    filterPairs (CodeBlock attr code, Para (Str "Caption:" : caption))
      | unprocessed attr =
        Just . single <$> compileCodeBlock attr code caption
    -- Any number of consecutive images in a masonry row.
    filterPairs (LineBlock lines, Para (Str "Caption:" : caption))
      | oneImagePerLine lines =
        Just . single <$> compileLineBlock (map (unpackImage . List.head) lines) caption
    -- Default filter
    filterPairs (x, y) = return Nothing
    filterTriplets :: (Block, Block, Block) -> Filter (Maybe [Block])
    -- Default filter
    filterTriplets (x, y, z) = return Nothing

unpackImage :: Inline -> (Attr, [Inline], Text, Text)
unpackImage (Image attr alt (url, title)) = (attr, alt, url, title)
unpackImage _ = error "Inline is not an Image. oneImagePerLine seems to have failed."

-- | Filters lists of Inlines that can match in pairs or triplets
mediaInlineListFilter :: [Inline] -> Filter [Inline]
mediaInlineListFilter inlines =
  tripletwise filterTriplets inlines >>= pairwise filterPairs
  where
    filterPairs :: (Inline, Inline) -> Filter (Maybe [Inline])
    -- Default filter
    filterPairs (x, y) = return Nothing
    -- Default filter
    filterTriplets (x, y, z) = return Nothing

-- | Match a single Block element
mediaBlockFilter :: Block -> Filter Block
-- A solitary image in a paragraph with a possible caption.
mediaBlockFilter (Para [Image attr alt (url, title)])
  | unprocessed attr =
    compileImage attr [] url title alt
-- A solitary code block in a paragraph with a possible caption.
mediaBlockFilter (CodeBlock attr code)
  | unprocessed attr =
    compileCodeBlock attr code []
-- Any number of consecutive images in a masonry row.
mediaBlockFilter (LineBlock lines)
  | oneImagePerLine lines =
    compileLineBlock (map (unpackImage . List.head) lines) []
-- Default filter
mediaBlockFilter block = return block

-- | Matches a single Inline element
mediaInlineFilter :: Inline -> Filter Inline
-- An inline image with a possible caption.
mediaInlineFilter (Image attr alt (url, title))
  | unprocessed attr =
    compileImage attr [] url title alt
-- transformImage image caption
-- Default filter
mediaInlineFilter inline = return inline

unprocessed :: Attr -> Bool
unprocessed (_, cls, _) = "processed" `notElem` cls

-- | Runs a filter on a Pandoc document. The options are used to rewrite document
-- fragments to HTML or back to Markdown. The meta data may be transformed by
-- the filter. The filter runs in the Filter monad and has access to options
-- and meta data via `gets` and `puts`.
runFilter2 ::
  Walkable a Pandoc =>
  Disposition ->
  WriterOptions ->
  (a -> Filter a) ->
  Pandoc ->
  IO Pandoc
runFilter2 dispo options filter pandoc@(Pandoc meta _) = do
  (Pandoc _ blocks, FilterState _ meta dispo) <-
    runStateT (walkM filter pandoc) (FilterState options meta dispo)
  return $ Pandoc meta blocks
