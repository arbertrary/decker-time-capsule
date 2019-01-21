{-- Author: Henrik Tramberend <henrik@tramberend.de> --}
import Common
import Exception
import External
import Flags (hasPreextractedResources)
import Meta
import NewResources (handleResources)
import Output
import Project
import Resources
import Shake
import Utilities

import Control.Exception
import Control.Lens ((^.))
import Control.Monad (when)
import Control.Monad.Extra
import Data.Aeson
import Data.IORef ()
import Data.List
import Data.Maybe
import Data.String ()
import Data.Version
import Development.Shake
import Development.Shake.FilePath
import GHC.Conc (numCapabilities)
import System.Decker.OS (defaultProvisioning)
import System.Directory (createDirectoryIfMissing, createFileLink, removeFile)
import System.Environment.Blank
import System.FilePath ()
import Text.Groom
import qualified Text.Mustache as M ()
import Text.Pandoc
import Text.Pandoc.Definition
import Text.Printf

main :: IO ()
main = do
  when isDevelopmentVersion $
    printf
      "WARNING: You are running a development build of decker (version: %s, branch: %s, commit: %s, tag: %s). Please be sure that you know what you're doing.\n"
      deckerVersion
      deckerGitBranch
      deckerGitCommitId
      deckerGitVersionTag
  extractResources
  directories <- projectDirectories
  --
  let serverPort = 8888
  let serverUrl = "http://localhost:" ++ (show serverPort)
  let indexSource = (directories ^. project) </> "index.md"
  let index = (directories ^. public) </> "index.html"
  let cruft = ["index.md.generated", "log", "//.shake", "generated", "code"]
  --
  runDecker $
  --
   do
    want ["html"]
    --
    phony "version" $ do
      putNormal $
        "decker version " ++
        deckerVersion ++
        " (branch: " ++
        deckerGitBranch ++
        ", commit: " ++
        deckerGitCommitId ++ ", tag: " ++ deckerGitVersionTag ++ ")"
      putNormal $ "pandoc version " ++ pandocVersion
      putNormal $ "pandoc-types version " ++ showVersion pandocTypesVersion
    --
    phony "decks" $ do
      need ["index"]
      decksA >>= need
    --
    phony "html" $ do
      need ["index"]
      allHtmlA >>= need
    --
    phony "pdf" $ do
      need ["index"]
      allPdfA >>= need
    --
    phony "pdf-decks" $ do
      need ["index"]
      decksPdfA >>= need
    --
    phony "watch" $ do
      need ["html"]
      watchChangesAndRepeat
    --
    phony "open" $ do
      need ["html"]
      openBrowser index
    --
    phony "server" $ do
      need ["watch"]
      runHttpServer serverPort directories Nothing
    --
    phony "example" $ liftIO writeExampleProject
    --
    phony "sketch-pad-index" $ do
      indicesA >>= need
      indicesA >>=
        writeSketchPadIndex ((directories ^. public) </> "sketch-pad.yaml")
    --
    phony "index" $ need ["support", index]
    --
    priority 2 $
      "//*-deck.html" %> \out -> do
        src <- calcSource "-deck.html" "-deck.md" out
        let ind = replaceSuffix "-deck.html" "-deck-index.yaml" out
        markdownToHtmlDeck src out ind
    --
    priority 2 $
      "//*-deck-index.yaml" %> \ind -> do
        src <- calcSource "-deck-index.yaml" "-deck.md" ind
        let out = replaceSuffix "-deck-index.yaml" "-deck.html" ind
        markdownToHtmlDeck src out ind
    --
    priority 2 $
      "//*-deck.pdf" %> \out -> do
        let src = replaceSuffix "-deck.pdf" "-deck.html" out
        need [src]
        putNormal $ src ++ " -> " ++ out
        runHttpServer serverPort directories Nothing
        decktape [serverUrl </> makeRelative (directories ^. public) src, out]
    --
    priority 2 $
      "//*-handout.html" %> \out -> do
        src <- calcSource "-handout.html" "-deck.md" out
        markdownToHtmlHandout src out
    --
    priority 2 $
      "//*-handout.pdf" %> \out -> do
        src <- calcSource "-handout.pdf" "-deck.md" out
        markdownToPdfHandout src out
    --
    priority 2 $
      "//*-page.html" %> \out -> do
        src <- calcSource "-page.html" "-page.md" out
        markdownToHtmlPage src out
    --
    priority 2 $
      "//*-page.pdf" %> \out -> do
        src <- calcSource "-page.pdf" "-page.md" out
        markdownToPdfPage src out
    --
    priority 2 $
      index %> \out -> do
        exists <- Development.Shake.doesFileExist indexSource
        let src =
              if exists
                then indexSource
                else indexSource <.> "generated"
        markdownToHtmlPage src out
    --
    indexSource <.> "generated" %> \out ->
      writeIndexLists out (takeDirectory index)
    --
    priority 2 $
      "//*.dot.svg" %> \out -> do
        let src = dropExtension out
        need [src]
        dot [("-o" ++ out), src]
    --
    priority 2 $
      "//*.gnuplot.svg" %> \out -> do
        let src = dropExtension out
        need [src]
        gnuplot ["-e", "set output \"" ++ out ++ "\"", src]
    --
    priority 2 $
      "//*.tex.svg" %> \out -> do
        let src = dropExtension out
        let pdf = src -<.> ".pdf"
        let dir = takeDirectory src
        need [src]
        pdflatex ["-output-directory", dir, src]
        pdf2svg [pdf, out]
        liftIO $ removeFile pdf
    --
    -- | cleans the local project (remove "public" folder and other generated files)
    phony "clean" $ do
      removeFilesAfter (directories ^. public) ["//"]
      removeFilesAfter (directories ^. project) cruft
    --
    -- | deletes old, cached resource folders
    -- TODO: include clear-cache in makefile?
    phony "clear-cache" $ do
      old <- liftIO oldResourcePaths
      forM_ old $ \dir -> removeFilesAfter dir ["//"]
      when (isDevelopmentVersion && not hasPreextractedResources) $
        removeFilesAfter (directories ^. appData) ["//"]
    --
    phony "help" $ do
      text <- liftIO $ getResourceString "template/help-page.md"
      liftIO $ putStr text
    --
    phony "resources" $ do
      metaData <- metaA
      liftIO $ handleResources metaData
      -- liftIO $ print (handleResources metaData)
    --
    phony "info" $ do
      putNormal $ "\nproject directory: " ++ (directories ^. project)
      putNormal $ "public directory: " ++ (directories ^. public)
      putNormal $ "support directory: " ++ (directories ^. support)
      putNormal $ "application data directory: " ++ (directories ^. appData)
      putNormal "\ntargets:\n"
      allHtmlA <++> allPdfA >>= mapM_ putNormal
      putNormal "\ntop level meta data:\n"
      groom <$> metaA >>= putNormal
    --
    -- TODO: Maybe move parts of this to Resources?
    phony "support" $ do
      metaData <- metaA
      unlessM (Development.Shake.doesDirectoryExist (directories ^. support)) $ do
        liftIO $ createDirectoryIfMissing True (directories ^. public)
        case metaValueAsString "provisioning" metaData of
          Just value
            | value == show SymLink ->
              liftIO $
              createFileLink
                ((directories ^. appData) </> "support")
                (directories ^. support)
          Just value
            | value == show Copy ->
              liftIO $
              copyDir
                ((directories ^. appData) </> "support")
                (directories ^. support)
          Nothing ->
            liftIO $
            case defaultProvisioning of
              SymLink ->
                createFileLink
                  ((directories ^. appData) </> "support")
                  (directories ^. support)
              _ ->
                copyDir
                  ((directories ^. appData) </> "support")
                  (directories ^. support)
          _ -> return ()
    --
    phony "check" checkExternalPrograms
    --
    phony "publish" $ do
      need ["index"]
      allHtmlA >>= need
      metaData <- metaA
      let host = metaValueAsString "rsync-destination.host" metaData
      let path = metaValueAsString "rsync-destination.path" metaData
      if isJust host && isJust path
        then do
          let src = (directories ^. public) ++ "/"
          let dst = intercalate ":" [fromJust host, fromJust path]
          ssh [(fromJust host), "mkdir -p", (fromJust path)]
          rsync [src, dst]
        else throw RsyncUrlException
