import Control.Exception
import Control.Monad ()
import qualified Data.ByteString.Char8 as B
import Data.IORef ()
import Data.List
import Data.Maybe
import Data.String ()
import Data.Yaml.Pretty
import Development.Shake
import Development.Shake.FilePath
import System.Directory
import System.Exit
import System.FilePath ()
import System.FilePath.Glob
import qualified Text.Mustache as M ()
import Text.Pandoc ()
import Text.Printf ()
import Utilities
import Context
import Embed

globA :: FilePattern -> Action [FilePath]
globA pattern =
  do projectDir <- getProjectDir
     liftIO $ globDir1 (compile pattern) projectDir

main :: IO ()
main = do
    -- Calculate some directories
    projectDir <- calcProjectDirectory
    let publicDir = projectDir </> publicDirName
    let cacheDir = publicDir </> "cache"
    let supportDir = publicDir </> "support"

    -- Find sources. These are formulated as actions in the Action mondad, such
    -- that each new iteration rescans all possible source files.
    let deckSourcesA = globA "**/*-deck.md"
    let pageSourcesA = globA "**/*-page.md"
    deckSources <- glob "**/*-deck.md"
    pageSources <- glob "**/*-page.md"
    let allSourcesA = deckSourcesA <++> pageSourcesA
    let allSources = deckSources ++ pageSources

    let metaA = globA "**/*-meta.yaml"
    meta <- glob "**/*-meta.yaml"

    -- Read meta data.
    -- metaData <- readMetaDataIO meta

    -- Calculate targets
    let decksA = deckSourcesA >>= calcTargets ".md" ".html"
    let decks = targetPathes deckSources projectDir ".md" ".html"
    let decksPdfA = deckSourcesA >>= calcTargets ".md" ".pdf"
    let decksPdf = targetPathes deckSources projectDir ".md" ".pdf"
    let handoutsA = deckSourcesA >>= calcTargets "-deck.md" "-handout.html"
    let handouts = targetPathes deckSources projectDir "-deck.md" "-handout.html"
    let handoutsPdfA = deckSourcesA >>= calcTargets "-deck.md" "-handout.pdf"
    let handoutsPdf = targetPathes deckSources projectDir "-deck.md" "-handout.pdf"
    let pagesA = pageSourcesA >>= calcTargets ".md" ".html"
    let pages = targetPathes pageSources projectDir ".md" ".html"
    let pagesPdfA = pageSourcesA >>= calcTargets ".md" ".pdf"
    let pagesPdf = targetPathes pageSources projectDir ".md" ".pdf"

    let indexSource = projectDir </> "index.md"
    let index = publicDir </> "index.html"
    let indexA = return [index] :: Action [FilePath]

    let everythingA = decksA <++> handoutsA <++> pagesA
    let everythingPdfA = decksPdfA <++> handoutsPdfA <++> pagesPdfA
    let everything = decks ++ handouts ++ pages
    let everythingPdf = decksPdf ++ handoutsPdf ++ pagesPdf

    let cruft = map (combine projectDir) [ "index.md.generated"
                                         , "server.log"
                                         , "//.shake"
                                         ]

    context <- makeActionContext projectDir publicDir cacheDir supportDir
    runShakeInContext context options $ do

        want ["html"]

        phony "decks" $ do
            decksA >>= need

        phony "html" $ do
            -- need $ everything ++ [index]
            everythingA <++> indexA >>= need

        phony "pdf" $ do
            -- need $ pagesPdf ++ handoutsPdf ++ [index]
            pagesPdfA <++> handoutsPdfA <++> indexA >>= need

        phony "pdf-decks" $ do
            -- need $ decksPdf ++ [index]
            decksPdfA <++> indexA >>= need

        phony "watch" $ do
            need ["html"]
            -- watchFiles $ allSources ++ meta
            allSourcesA <++> metaA >>= watchFiles

        phony "server" $ do
            need ["watch"]
            runHttpServer publicDir True

        phony "example" writeExampleProject

        priority 2 $ "//*-deck.html" %> \out -> do
            -- let src = sourcePath out projectDir ".html" ".md"
            src <- calcSource "-deck.html" "-deck.md" out
            metaData <- metaA >>= readMetaData -- TODO new readMetaData
            markdownToHtmlDeck src metaData out

        priority 2 $ "//*-deck.pdf" %> \out -> do
            -- let src = sourcePath out projectDir ".pdf" ".html"
            let src = replaceSuffix "-deck.pdf" "-deck.html" out
            runHttpServer publicDir False
            code <- cmd "decktape.sh reveal" ("http://localhost:8888/" ++ (makeRelative projectDir src)) out
            case code of
              ExitFailure _ -> do
                 throw $ DecktapeException "Unknown."
              ExitSuccess ->
                 return ()

        priority 2 $ "//*-handout.html" %> \out -> do
            -- let src = sourcePath out projectDir "-handout.html" "-deck.md"
            src <- calcSource "-handout.html" "-deck.md" out
            meta <- metaA
            need meta
            markdownToHtmlHandout src meta out  -- TODO new readMetaData

        priority 2 $ "//*-handout.pdf" %> \out -> do
            src <- calcSource "-handout.pdf" "-deck.md" out
            meta <- metaA
            need meta
            markdownToPdfHandout src meta out

        priority 2 $ "//*-page.html" %> \out -> do
            -- let src = sourcePath out projectDir "-page.html" "-page.md"
            src <- calcSource "-page.html" "-page.md" out
            metaData <- metaA >>= readMetaData
            markdownToHtmlPage src metaData out

        priority 2 $ "//*-page.pdf" %> \out -> do
            -- let src = sourcePath out projectDir "-page.pdf" "-page.md"
            src <- calcSource "-page.pdf" "-page.md" out
            metaData <- metaA >>= readMetaData
            markdownToPdfPage src metaData out

        priority 2 $ index %> \out -> do
            exists <- Development.Shake.doesFileExist indexSource
            let src = if exists then indexSource else indexSource <.> "generated"
            need [src]
            rel <- getRelativeSupportDir out
            metaData <- metaA >>= readMetaData
            markdownToHtmlPage src metaData out

        indexSource <.> "generated" %> \out -> do
            writeIndex out (takeDirectory index) decks handouts pages

        phony "clean" $ do
            removeFilesAfter publicDir ["//"]
            removeFilesAfter projectDir cruft

        phony "help" $
            liftIO $ putStr deckerHelpText

        phony "plan" $ do
            putNormal $ "project directory: " ++ projectDir
            putNormal $ "public directory: " ++ publicDir
            putNormal $ "support directory: " ++ supportDir
            putNormal "meta:"
            metaA >>= mapM_ putNormal
            putNormal "sources:"
            allSourcesA >>= mapM_ putNormal
            putNormal "targets:"
            everythingA <++> everythingPdfA >>= mapM_ putNormal

        phony "meta" $ do
            metaData <- metaA >>= readMetaData
            liftIO $ B.putStr $ encodePretty defConfig metaData

        phony "support" $ do
            writeEmbeddedFiles deckerSupportDir supportDir

        phony "publish" $ do
            everythingA <++> indexA >>= need
            metaData <- metaA >>= readMetaData
            let host = metaValueAsString "rsync-destination.host" metaData
            let path = metaValueAsString "rsync-destination.path" metaData
            if isJust host && isJust path
               then do
                   cmd "ssh " (fromJust host) "mkdir -p" (fromJust path) :: Action ()
                   cmd "rsync -a" publicDir $ intercalate ":" [fromJust host, fromJust path] :: Action ()
               else throw RsyncUrlException

        phony "cache" $ do
            meta <- metaA
            cacheRemoteImages cacheDir meta allSources

        phony "clean-cache" $ do
            need ["clean"]
            removeFilesAfter "." ["**/cached"]

-- | Some constants that might need tweaking
resourceDir = "img"
options = shakeOptions{shakeFiles=".shake"}

publicDirName :: String
publicDirName = "public"

targetPath :: FilePath -> FilePath -> String -> String -> FilePath
targetPath source projectDir srcSuffix targetSuffix =
  let target = projectDir </> publicDirName </> (makeRelative projectDir source)
  in dropSuffix srcSuffix target ++ targetSuffix

replaceSuffix srcSuffix targetSuffix filename = dropSuffix srcSuffix filename ++ targetSuffix

-- | Calculates the target pathes from a list of source files.
calcTargets :: String -> String -> [FilePath] -> Action [FilePath]
calcTargets srcSuffix targetSuffix sources =
  do projectDir <- getProjectDir
     publicDir <- getPublicDir
     return $ map (replaceSuffix srcSuffix targetSuffix . combine publicDir . makeRelative projectDir) sources

targetPathes :: [FilePath] -> FilePath -> String -> String -> [FilePath]
targetPathes sources projectDir srcSuffix targetSuffix =
  [targetPath s projectDir srcSuffix targetSuffix | s <- sources]

-- | Calculate the source file from the target path. Calls need.
calcSource :: String -> String -> FilePath -> Action FilePath
calcSource targetSuffix srcSuffix target =
  do projectDir <- getProjectDir
     publicDir <- getPublicDir
     let src = (replaceSuffix targetSuffix srcSuffix . combine projectDir .  makeRelative publicDir) target
     need [src]
     return src

sourcePath :: FilePath -> FilePath -> String -> String -> FilePath
sourcePath out projectDir targetSuffix srcSuffix =
  let source = projectDir </> (makeRelative (projectDir </> publicDirName) out)
  in dropSuffix targetSuffix source ++ srcSuffix
