import IncludeTests
import MetaTests
import SketchTests
import Test.Hspec
import WatchTests

import Control.Lens ((^.))
import qualified Data.ByteString.Char8 as B
import qualified Data.HashMap.Strict as H
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Text
import Data.Text.Encoding
import qualified Data.Yaml as Y
import qualified System.Directory as Dir
import System.FilePath
import System.FilePath.Glob
import Text.Decker.Filter.Filter
import Text.Decker.Internal.Helper as Help
import Text.Decker.Project.Project as P
import Text.Decker.Resource.Resource
import Text.Pandoc

main = do
  dirs <- projectDirectories
  --
  deckTemplate <- B.readFile (dirs ^. project </> "resource/template/deck.html")
  --
  -- metaFiles <- globDir1 (compile "**/*-meta.yaml") (dirs ^. project)
  let metaFile = (dirs ^. project) </> "decker.yaml"
  --
  hspec $
  --
   do
    includeTests
    -- watchTests
    -- sketchTests
    metaTests
    describe "makeRelativeTo" $
      it "calculates the path of file relative to dir. Includes '..'" $ do
        makeRelativeTo "" "img.png" `shouldBe` "img.png"
        makeRelativeTo "/one/two" "/one/two/img.png" `shouldBe` "img.png"
        makeRelativeTo "/one/two/three" "/one/two/four/img.png" `shouldBe`
          joinPath ["..", "four", "img.png"]
        makeRelativeTo "/some/where/else" "/one/two/four/img.png" `shouldBe`
          joinPath ["..", "..", "..", "one", "two", "four", "img.png"]
        makeRelativeTo
          "/Users/henrik/tmp/decker-demo/public"
          "/Users/henrik/tmp/decker-demo/public/cache/b48cadafb942dc1426316772321dd0c7.png" `shouldBe`
          joinPath ["cache", "b48cadafb942dc1426316772321dd0c7.png"]
    --
    describe "removeCommonPrefix" $
      it "removes the common prefix from two pathes." $ do
        Help.removeCommonPrefix ("", "") `shouldBe` ("", "")
        Help.removeCommonPrefix ("fasel/bla", "fasel/bla/lall") `shouldBe`
          ("", "lall")
        Help.removeCommonPrefix ("lurgel/hopp", "fasel/bla/lall") `shouldBe`
          (joinPath ["lurgel", "hopp"], joinPath ["fasel", "bla", "lall"])
        Help.removeCommonPrefix ("/lurgel/hopp", "fasel/bla/lall") `shouldBe`
          (joinPath ["/lurgel", "hopp"], joinPath ["fasel", "bla", "lall"])
        Help.removeCommonPrefix ("/lurgel/hopp", "/fasel/bla/lall") `shouldBe`
          (joinPath ["lurgel", "hopp"], joinPath ["fasel", "bla", "lall"])
    --
    describe "copyResource" $
      it
        "copies an existing resource to the public dir and returns the public URL." $ do
        Dir.doesFileExist
          ((dirs ^. project) </> "resource/example/img/haskell.png") `shouldReturn`
          True
        copyResource
          (Resource
             ((dirs ^. project) </> "resource/example/img/haskell.png")
             ((dirs ^. public) </> "resource/example/img/haskell.png")
             "img/haskell.png") `shouldReturn`
          "img/haskell.png"
        Dir.doesFileExist
          ((dirs ^. public) </> "resource/example/img/haskell.png") `shouldReturn`
          True
    --
    describe "linkResource" $
      it
        "links an existing resource to the public dir and returns the public URL." $ do
        Dir.doesFileExist
          ((dirs ^. project) </> "resource/example/img/haskell.png") `shouldReturn`
          True
        linkResource
          (Resource
             ((dirs ^. project) </> "resource/example/img/haskell.png")
             ((dirs ^. public) </> "resource/example/img/haskell.png")
             "img/haskell.png") `shouldReturn`
          "img/haskell.png"
        Dir.pathIsSymbolicLink
          ((dirs ^. public) </> "resource/example/img/haskell.png") `shouldReturn`
          True
    --
    describe "convertMediaAttributes" $
      it
        "transfers 'width' and 'height' attribute values to css style values and add them to the 'style' attribute value." $ do
        convertMediaAttributes ("", [], [("width", "100%")]) `shouldBe`
          ("", [], [("style", "width:100%;")])
        convertMediaAttributes ("", [], [("height", "50%")]) `shouldBe`
          ("", [], [("style", "height:50%;")])
        convertMediaAttributes
          ("", [], [("width", "100%"), ("style", "color:red;")]) `shouldBe`
          ("", [], [("style", "color:red;width:100%;")])

meta :: Meta
meta =
  Meta $
  M.fromList
    [ ("top-level-key", MetaString "top-level-value")
    , ( "group"
      , MetaMap $ M.fromList [("attribute", MetaString "attribute-value")])
    ]
