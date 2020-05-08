{-- Author: Henrik Tramberend <henrik@tramberend.de> --}
module Decker where

import Text.Decker.Internal.Common
import Text.Decker.Internal.External
import Text.Decker.Internal.Helper
import Text.Decker.Internal.Meta
import Text.Decker.Project.Project
import Text.Decker.Project.Shake
import Text.Decker.Project.Version
import Text.Decker.Resource.Resource
import Text.Decker.Resource.Template

import Text.Decker.Writer.Html
import Text.Decker.Writer.Pdf

-- TODO Is this still used?
--import Text.Decker.Server.Dachdecker
import Control.Concurrent
import Control.Lens ((^.))
import Control.Monad.Extra
import Data.IORef ()
import Data.List
import Data.String ()
import qualified Data.Text as Text
import Data.Version
import Development.Shake
import Development.Shake.FilePath
import qualified System.Directory as Dir
import System.Environment.Blank
import System.IO
import Text.Groom
import qualified Text.Mustache as M ()
import Text.Pandoc hiding (lookupMeta)

main :: IO ()
main = do
  args <- getArgs
  if null args
    then run
    else case head args of
           "example" -> writeExampleProject
           "tutorial" -> writeTutorialProject
           _ -> run

type ParamCache a = FilePath -> Action a

type Cache a = Action a

prepCaches ::
     ProjectDirs
  -> Rules (Cache Meta, Cache Targets, ParamCache (Template Text.Text))
prepCaches directories = do
  let deckerMetaFile = (directories ^. project) </> "decker.yaml"
  let deckerTargetsFile = (directories ^. transient) </> "targets.yaml"
  getGlobalMeta <- ($ deckerMetaFile) <$> newCache (readStaticMetaData directories)
  getTargets <- ($ deckerTargetsFile) <$> newCache readTargetsFile
  getTemplate <-
    newCache
      (\path -> do
         meta <- getGlobalMeta
         readTemplate meta path)
  deckerTargetsFile %> \targetFile -> do
    alwaysRerun
    meta <- getGlobalMeta
    scanTargetsToFile meta directories targetFile
  return (getGlobalMeta, getTargets, getTemplate)

needSel sel = needSels [sel]

needSels sels targets = need (concatMap (targets ^.) sels)

run :: IO ()
run = do
  warnVersion
  directories <- projectDirectories
  --
  let serverPort = 8888
  let serverUrl = "http://localhost:" ++ show serverPort
  let indexSource = (directories ^. project) </> "index.md"
  let generatedIndexSource = (directories ^. transient) </> "index.md.generated"
  let indexFile = (directories ^. public) </> "index.html"
  let cruft = ["//" <> deckerFiles]
  let pdfMsg =
        "\n# To use 'decker pdf' or 'decker pdf-decks', Google Chrome has to be installed.\n" ++
        "# Windows: Currently 'decker pdf' does not work on Windows.\n" ++
        "\tPlease add 'print: true' or 'menu: true' to your slide deck and use the print button on the title slide.\n" ++
        "# MacOS: Follow the Google Chrome installer instructions.\n" ++
        "\tGoogle Chrome.app has to be located in either /Applications/Google Chrome.app or /Users/<username>/Applications/Google Chrome.app\n" ++
        "\tAlternatively you can add 'chrome' to $PATH.\n" ++
        "# Linux: 'chrome' has to be on $PATH.\n"
  --
  runDecker $ do
    (getGlobalMeta, getTargets, getTemplate) <- prepCaches directories
    --
    want ["decks"]
    --
    phony "version" $ do
      putNormal $
        "decker version " ++
        deckerVersion ++
        " (branch: " ++
        deckerGitBranch ++
        ", commit: " ++
        deckerGitCommitId ++ ", tag: " ++ deckerGitVersionTag ++ ")"
      putNormal $ "pandoc version " ++ Text.unpack pandocVersion
      putNormal $ "pandoc-types version " ++ showVersion pandocTypesVersion
    --
    phony "decks" $ do
      need ["support"]
      getTargets >>= needSel decks
    --
    phony "html" $ do
      need ["support"]
      getTargets >>= needSels [decks, pages, handouts]
    --
    phony "pdf" $ do
      putNormal pdfMsg
      need ["support"]
      getTargets >>= needSels [decksPdf, handoutsPdf, pagesPdf]
    --
    phony "pdf-decks" $ do
      putNormal pdfMsg
      need ["support"]
      getTargets >>= needSel decksPdf
    --
    phony "watch" $ do
      need ["html"]
      watchChangesAndRepeat
    --
    phony "open" $ do
      need ["html"]
      openBrowser indexFile
    --
    phony "server" $ do
      need ["watch", "support"]
      runHttpServer serverPort directories Nothing
    --
    phony "presentation" $ do
      need ["support"]
      runHttpServer serverPort directories Nothing
      liftIO waitForYes
    --
    phony "fast" $ do
      need ["support"]
      runHttpServer serverPort directories Nothing
      pages <- currentlyServedPages
      need $ map (directories ^. public </>) pages
      watchChangesAndRepeat
    --
    alternatives $ do
      (directories ^. public) <//> "*-deck.html" %> \out -> do
        meta <- getGlobalMeta
        let src = sourceForTarget meta out
        need [src]
        let annotDst = replaceSuffix "-deck.html" "-annot.json" out
        let annotSrc = sourceForTarget meta annotDst
        exists <- liftIO $ Dir.doesFileExist annotSrc
        when exists $ need [annotDst]
        markdownToHtmlDeck meta getTemplate src out
      --
      (directories ^. public) <//> "*-deck.pdf" %> \out -> do
        meta <- getGlobalMeta
        let src = sourceForTarget meta out
        need [src]
        putNormal $ "Started: " ++ src ++ " -> " ++ out
        runHttpServer serverPort directories Nothing
        result <-
          liftIO $
          launchChrome
            (serverUrl </> makeRelative (directories ^. public) src)
            out
        case result of
          Right msg -> putNormal msg
          Left msg -> error msg
      --
      (directories ^. public) <//> "*-handout.html" %> \out -> do
        meta <- getGlobalMeta
        let src = sourceForTarget meta out
        need [src]
        markdownToHtmlHandout meta getTemplate src out
      --
      (directories ^. public) <//> "*-handout.pdf" %> \out -> do
        meta <- getGlobalMeta
        let src = sourceForTarget meta out
        need [src]
        markdownToPdfHandout meta getTemplate src out
      --
      (directories ^. public) <//> "*-page.html" %> \out -> do
        meta <- getGlobalMeta
        let src = sourceForTarget meta out
        need [src]
        markdownToHtmlPage meta getTemplate src out
      --
      (directories ^. public) <//> "*-page.pdf" %> \out -> do
        meta <- getGlobalMeta
        let src = sourceForTarget meta out
        need [src]
        markdownToPdfPage meta getTemplate src out
      --
      (directories ^. public) <//> "*-annot.json" %> \out -> do
        meta <- getGlobalMeta
        let src = sourceForTarget meta out
        need [src]
        putNormal $ "# copy (for " <> out <> ")"
        copyFile' src out
      --
      indexFile %> \out -> do
        exists <- doesFileExist indexSource
        let src =
              if exists
                then indexSource
                else generatedIndexSource
        meta <- getGlobalMeta
        markdownToHtmlPage meta getTemplate src out
      --
      generatedIndexSource %> \out -> do
        targets <- getTargets
        meta <- getGlobalMeta
        writeIndexLists meta targets out (takeDirectory indexFile)
      --
      (directories ^. project) <//> "*.dot.svg" %> \out -> do
        let src = dropExtension out
        need [src]
        dot ["-o" ++ out, src]
      --
      (directories ^. project) <//> "*.gnuplot.svg" %> \out -> do
        let src = dropExtension out
        need [src]
        gnuplot ["-e", "set output \"" ++ out ++ "\"", src]
      --
      (directories ^. project) <//> "*.tex.svg" %> \out -> do
        let src = dropExtension out
        let pdf = src -<.> ".pdf"
        let dir = takeDirectory src
        need [src]
        pdflatex ["-output-directory", dir, src]
        pdf2svg [pdf, out]
        liftIO $ Dir.removeFile pdf
      --
      -- Catch all. Just copy project/* to public/*. This nicely handles ALL
      -- resources. Just `need` them where you need them.
      (directories ^. public) <//> "//" %> \out -> do
        let src =
              (directories ^. project) </>
              makeRelative (directories ^. public) out
        copyFile' src out
    --
    phony "static-files" $ do
      targets <- getTargets
      need (targets ^. static)
    --
    phony "annotations" $ do
      targets <- getTargets
      need (targets ^. annotations)
    --
    phony "clean" $ do
      removeFilesAfter (directories ^. public) ["//"]
      removeFilesAfter (directories ^. project) cruft
    --
    phony "info" $ do
      putNormal $ "\nproject directory: " ++ (directories ^. project)
      putNormal $ "public directory: " ++ (directories ^. public)
      putNormal $ "support directory: " ++ (directories ^. support)
      meta <- getGlobalMeta
      targets <- getTargets
      templateSource <-
        liftIO $ calcTemplateSource (lookupMeta "template-source" meta)
      putNormal $ "template source: " <> show templateSource
      putNormal "\ntargets:\n"
      putNormal (groom targets)
      putNormal "\ntop level meta data:\n"
      putNormal (groom meta)
    --
    phony "support" $ do
      need [indexFile, "static-files", "annotations"]
      meta <- getGlobalMeta
      writeSupportFilesToPublic meta
    --
    phony "check" checkExternalPrograms
    --
    phony "publish" $ do
      need ["support"]
      meta <- getGlobalMeta
      getTargets >>= needSels [decks, handouts, pages]
      let host = lookupMetaOrFail "rsync-destination.host" meta
      let path = lookupMetaOrFail "rsync-destination.path" meta
      let src = (directories ^. public) ++ "/"
      let dst = intercalate ":" [host, path]
      ssh [host, "mkdir -p", path]
      rsync [src, dst]
    -- TODO Is this still needed?
    --phony "sync" $ uploadQuizzes (_sources <$> targetsA)

waitForYes :: IO ()
waitForYes = do
  threadDelay 1000
  putStr "\nDecker server running. Push ENTER to terminate."
  hFlush stdout
  _ <- getLine
  putStr "Terminate server? (y/N): "
  hFlush stdout
  input <- getLine
  unless (input == "y") waitForYes
