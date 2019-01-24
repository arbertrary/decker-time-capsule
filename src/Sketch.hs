module Sketch
  ( randomId
  , writeToMarkdownFile
  , provideSlideIds
  , provideSlideId
  , provideSlideIdIO
  , idDigits
  ) where

import Common
import Markdown
import Meta
import Resources
import Slide

import Control.Lens
import Control.Monad
import Data.Maybe
import qualified Data.Text.IO as T
import System.Directory
import System.FilePath
import System.IO
import System.IO.Temp
import System.Random
import Text.Pandoc
import Text.Pandoc.Lens
import Text.Pandoc.Shared
import Text.Printf
import Text.Read

idDigits = 4

-- | Selects a random id out of idDigits^36 possibilities
randomId :: IO String
randomId = do
  h <- randomAlpha
  t <- replicateM (idDigits - 1) randomAlphaNum
  return $ h : t

-- | Rejection sampling for a random character from [0-9] or [a-z].
randomAlphaNum :: IO Char
randomAlphaNum = do
  r <- getStdRandom (randomR ('0', 'z'))
  if r > '9' && r < 'a'
    then randomAlphaNum
    else return r

randomAlpha :: IO Char
randomAlpha = getStdRandom (randomR ('a', 'z'))

-- | Writes a pandoc document atoimically to a markdown file. It uses a modified
-- Markdown writer that produces more appropriately formatted documents.
writeToMarkdownFile :: FilePath -> Pandoc -> IO ()
writeToMarkdownFile filepath pandoc@(Pandoc meta _) = do
  template <- getResourceString $ "template" </> "deck.md"
  let columns = lookupInt "write-back.line-columns" 80 meta
  let wrapOpt "none" = WrapNone
      wrapOpt "preserve" = WrapPreserve
      wrapOpt _ = WrapAuto
  let wrap = lookupString "write-back.line-wrap" "auto" meta
  let extensions =
        (disableExtension Ext_simple_tables .
         disableExtension Ext_multiline_tables .
         enableExtension Ext_auto_identifiers)
          pandocExtensions
  let options =
        def
          { writerTemplate = Just template
          , writerExtensions = extensions
          , writerColumns = columns
          , writerWrapText = wrapOpt wrap
          , writerSetextHeaders = False
          }
  markdown <- runIO (Markdown.writeMarkdown options pandoc) >>= handleError
  fileContent <- T.readFile filepath
  when (markdown /= fileContent) $
    withTempFile
      (takeDirectory filepath)
      (takeFileName filepath)
      (\tmp h -> do
         T.hPutStr h markdown
         hFlush h
         renameFile tmp filepath)

provideSlideIds :: Pandoc -> IO Pandoc
provideSlideIds (Pandoc meta body) = do
  let slides = toSlides body
  idSlides <- mapM provideSlideIdIO slides
  let idBody = fromSlides idSlides
  return $ Pandoc meta idBody

-- Provides unique, random, sticky ids for all slides.
provideSlideId :: Slide -> Decker Slide
provideSlideId = doIO . provideSlideIdIO

provideSlideIdIO :: Slide -> IO Slide
-- Create random ID if there is none
provideSlideIdIO (Slide (Just (Header 1 ("", c, kv) i)) body) = do
  sid <- randomId
  return $ Slide (Just $ Header 1 (sid, c, kv) i) body
-- Create random ID and a level 1 header if there is neither
provideSlideIdIO (Slide Nothing body) = do
  sid <- randomId
  return $ Slide (Just $ Header 1 (sid, [], []) []) body
-- Preserve existing ID
provideSlideIdIO slide@(Slide (Just (Header 1 (sid, c, kv) i)) body) =
  return slide
