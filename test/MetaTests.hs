module MetaTests
  ( metaTests
  ) where

import qualified Data.Map.Strict as M
import Test.Hspec
import Text.Decker.Internal.Meta
import Text.Pandoc

m1 =
  Meta
    (M.fromList
       [ ("bool", MetaBool True)
       , ( "write-back"
         , MetaMap
             (M.fromList
                [ ("line-columns", MetaString "80")
                , ("line-wrap", MetaString "none")
                ]))
       , ("top", MetaMap (M.fromList [("bool", MetaBool True)]))
       , ( "list"
         , MetaList
             [ MetaMap (M.fromList [("bool0", MetaBool True)])
             , MetaMap (M.fromList [("bool1", MetaBool True)])
             , MetaMap (M.fromList [("bool2", MetaBool True)])
             ])
       ])

metaTests = do
  describe "lookupMetaBool" $ do
    it "looks up a top-level boolean meta value" $
      lookupMetaBool "bool" m1 `shouldBe` Just True
    it "looks up a top-level boolean meta value" $
      lookupMetaBool "none" m1 `shouldBe` Nothing
    it "looks up a boolean meta value" $
      lookupMetaBool "top.bool" m1 `shouldBe` Just True
    it "looks up a boolean meta value" $
      lookupMetaBool "top.none" m1 `shouldBe` Nothing
    it "looks up a boolean meta value in list" $
      lookupMetaBool "list[2].bool2" m1 `shouldBe` Just True
  describe "lookupMetaInt" $
    it "looks up a top-level int meta value" $
    lookupMetaInt "write-back.line-columns" m1 `shouldBe` Just 80
  describe "lookupMetaString" $
    it "looks up a top-level int meta value" $
    lookupMetaString "write-back.line-wrap" m1 `shouldBe` Just "none"
