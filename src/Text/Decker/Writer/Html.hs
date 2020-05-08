{-- Author: Henrik Tramberend <henrik@tramberend.de> --}
module Text.Decker.Writer.Html
  ( writeIndexLists
  , markdownToHtmlDeck
  , markdownToHtmlHandout
  , markdownToHtmlPage
  ) where

import Text.Decker.Filter.Filter
import Text.Decker.Internal.Common
import Text.Decker.Internal.Meta
import Text.Decker.Project.Project
import Text.Decker.Project.Shake
import Text.Decker.Reader.Markdown
import Text.Decker.Resource.Template
import Text.Pandoc.Lens

import Control.Monad.State
import qualified Data.Map as M
import qualified Data.MultiMap as MM
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Development.Shake
import Development.Shake.FilePath as SFP
import Text.DocTemplates
import Text.Pandoc hiding (getTemplate, lookupMeta)
import Text.Pandoc.Highlighting
import Text.Printf

-- | Generates an index.md file with links to all generated files of interest.
writeIndexLists :: Meta -> Targets -> FilePath -> FilePath -> Action ()
writeIndexLists meta targets out baseUrl = do
  let projectDir = lookupMetaOrElse "decker.directories.project" "." meta
  let decks = zip (_decks targets) (_decksPdf targets)
  let handouts = zip (_handouts targets) (_handoutsPdf targets)
  let pages = zip (_pages targets) (_pagesPdf targets)
  decksLinks <- makeGroupedLinks projectDir decks
  handoutsLinks <- makeGroupedLinks projectDir handouts
  pagesLinks <- makeGroupedLinks projectDir pages
  liftIO $
    writeFile out $
    unlines
      [ "---"
      , "title: Generated Index"
      , "subtitle: " ++ projectDir
      , "---"
      , "# Slide decks"
      , unlines decksLinks
      , "# Handouts"
      , unlines handoutsLinks
      , "# Supporting Documents"
      , unlines pagesLinks
      ]
  where
    makeLink (html, pdf) = do
      pdfExists <- doesFileExist pdf
      if pdfExists
        then return $
             printf
               "-    [%s <i class='fab fa-html5'></i>](%s) [<i class='fas fa-file-pdf'></i>](%s)"
               (takeFileName html)
               (makeRelative baseUrl html)
               (makeRelative baseUrl pdf)
        else return $
             printf
               "-    [%s <i class='fab fa-html5'></i>](%s)"
               (takeFileName html)
               (makeRelative baseUrl html)
    makeGroupedLinks :: FilePath -> [(FilePath, FilePath)] -> Action [String]
    makeGroupedLinks project files =
      let grouped = MM.fromList (zip (map (takeDirectory . fst) files) files)
          renderGroup :: FilePath -> Action [String]
          renderGroup key =
            (printf "\n## %s:" (makeRelative project key) :) <$>
            mapM makeLink (MM.lookup key grouped)
       in concat <$> mapM renderGroup (MM.keys grouped)

-- | Write Pandoc in native format right next to the output file
writeNativeWhileDebugging :: FilePath -> String -> Pandoc -> Action ()
writeNativeWhileDebugging out mod doc =
  liftIO $
  runIO (writeNative pandocWriterOpts doc) >>= handleError >>=
  T.writeFile (out -<.> mod <.> ".hs")


-- | Write a markdown file to a HTML file using the page template.
markdownToHtmlDeck :: Meta -> TemplateCache -> FilePath -> FilePath -> Action ()
markdownToHtmlDeck meta getTemplate markdownFile out = do
  putCurrentDocument out
  let supportDir = relativeSupportDir meta (takeDirectory out)
  let disp = Disposition Deck Html
  pandoc@(Pandoc meta _) <- readAndProcessMarkdown meta markdownFile disp
  let highlightStyle =
        case lookupMeta "highlightjs" meta of
          Nothing -> Just pygments
          Just (_ :: T.Text) -> Nothing
  template <- getTemplate (templateFile disp)
  -- dachdeckerUrl' <- liftIO getDachdeckerUrl
  let options =
        pandocWriterOpts
          { writerSlideLevel = Just 1
          , writerSectionDivs = False
          , writerTemplate = Just template
          , writerHighlightStyle = highlightStyle
          , writerHTMLMathMethod =
              MathJax "Handled by reveal.js in the template"
          , writerVariables =
              Context $
              M.fromList
                [ ("decker-support-dir", SimpleVal $ Text 0 $ T.pack supportDir)
                -- , ("dachdecker-url", SimpleVal $ Text 0 $ T.pack dachdeckerUrl')
                ]
          , writerCiteMethod = Citeproc
          }
  writePandocFile "revealjs" options out pandoc
  when (lookupMetaOrElse False "write-notebook" meta) $
    markdownToNotebook meta markdownFile (out -<.> ".ipynb")
  writeNativeWhileDebugging out "filtered" pandoc

writePandocFile :: T.Text -> WriterOptions -> FilePath -> Pandoc -> Action ()
writePandocFile fmt options out pandoc =
  liftIO $
  runIO (writeRevealJs options pandoc) >>= handleError >>= T.writeFile out

-- | Write a markdown file to a HTML file using the page template.
markdownToHtmlPage :: Meta -> TemplateCache -> FilePath -> FilePath -> Action ()
markdownToHtmlPage meta getTemplate markdownFile out = do
  putCurrentDocument out
  let supportDir = relativeSupportDir meta (takeDirectory out)
  let disp = Disposition Page Html
  pandoc@(Pandoc docMeta _) <- readAndProcessMarkdown meta markdownFile disp
  template <- getTemplate (templateFile disp)
  let options =
        pandocWriterOpts
          { writerTemplate = Just template
          , writerSectionDivs = False
          , writerHighlightStyle = Just pygments
          , writerHTMLMathMethod =
              MathJax "Handled by reveal.js in the template"
          , writerVariables =
              Context $
              M.fromList
                [("decker-support-dir", SimpleVal $ Text 0 $ T.pack supportDir)]
          , writerCiteMethod = Citeproc
          , writerTableOfContents = lookupMetaOrElse False "show-toc" docMeta
          , writerTOCDepth = lookupMetaOrElse 1 "toc-depth" docMeta
          }
  writePandocFile "html5" options out pandoc

-- | Write a markdown file to a HTML file using the handout template.
markdownToHtmlHandout :: Meta -> TemplateCache -> FilePath -> FilePath -> Action ()
markdownToHtmlHandout meta getTemplate markdownFile out = do
  putCurrentDocument out
  let supportDir = relativeSupportDir meta (takeDirectory out)
  let disp = Disposition Handout Html
  pandoc@(Pandoc docMeta _) <-
    wrapSlidesinDivs <$> readAndProcessMarkdown meta markdownFile disp
  template <- getTemplate (templateFile disp)
  let options =
        pandocWriterOpts
          { writerTemplate = Just template
          , writerHighlightStyle = Just pygments
          , writerHTMLMathMethod =
              MathJax "Handled by reveal.js in the template"
          , writerVariables =
              Context $
              M.fromList
                [("decker-support-dir", SimpleVal $ Text 0 $ T.pack supportDir)]
          , writerCiteMethod = Citeproc
          , writerTableOfContents = lookupMetaOrElse False "show-toc" docMeta
          , writerTOCDepth = lookupMetaOrElse 1 "toc-depth"  docMeta
          }
  writePandocFile "html5" options out pandoc

-- | Write a markdown file to a HTML file using the page template.
markdownToNotebook :: Meta -> FilePath -> FilePath -> Action ()
markdownToNotebook meta markdownFile out = do
  putCurrentDocument out
  let supportDir = relativeSupportDir meta (takeDirectory out)
  let disp = Disposition Notebook Html
  pandoc@(Pandoc docMeta _) <-
    filterNotebookSlides <$> readAndProcessMarkdown meta markdownFile disp
  let options =
        pandocWriterOpts
          { writerTemplate = Nothing
          , writerHighlightStyle = Just pygments
          , writerVariables =
              Context $
              M.fromList
                [("decker-support-dir", SimpleVal $ Text 0 $ T.pack supportDir)]
          }
  writePandocFile "ipynb" options out pandoc
