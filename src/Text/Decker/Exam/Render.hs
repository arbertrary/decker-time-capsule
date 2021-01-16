{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Text.Decker.Exam.Render
  ( renderQuestion,
    renderCatalog,
  )
where

import Control.Exception
import Control.Lens hiding (Choice)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text.IO as Text
import Development.Shake hiding (Resource)
import Relude
import Relude.Extra.Group
import System.FilePath.Posix
import Text.Blaze.Html
import Text.Blaze.Html.Renderer.Pretty
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Decker.Exam.Question
import Text.Decker.Filter.Paths
import Text.Decker.Internal.Common
import Text.Decker.Internal.Meta
import Text.Decker.Reader.Markdown
-- import Text.Groom
import Text.Pandoc
import Text.Pandoc.Walk

compileQuestionToHtml :: Meta -> FilePath -> Question -> Action Question
compileQuestionToHtml meta base quest = do
  let render = renderSnippetToHtml meta base
  traverseOf qstTitle render
    =<< traverseOf qstQuestion render
    =<< traverseOf qstAnswer (compileAnswerToHtml meta base) quest

compileAnswerToHtml :: Meta -> FilePath -> Answer -> Action Answer
compileAnswerToHtml meta base mc@MultipleChoice {} = do
  let render = renderSnippetToHtml meta base
  traverseOf (answChoices . traverse . choiceTheAnswer) render mc
compileAnswerToHtml meta base ma@MultipleAnswers {} = do
  let render = renderSnippetToHtml meta base
  traverseOf (answAnswers . traverse . oneDetail) render
    =<< traverseOf (answAnswers . traverse . oneDetail) render ma
compileAnswerToHtml meta base ff@FreeForm {} = do
  let render = renderSnippetToHtml meta base
  traverseOf answCorrectAnswer render ff
compileAnswerToHtml meta base ft@FillText {} = do
  let render = renderSnippetToHtml meta base
  traverseOf (answCorrectWords . traverse) render ft

-- | Renders a Markdown snippet to HTML applying the full Decker media filter.
renderSnippetToHtml :: Meta -> FilePath -> Text -> Action Text
renderSnippetToHtml meta base markdown = do
  pandoc <- liftIO $ handleError $ runPure $ readMarkdown pandocReaderOpts markdown
  let options = pandocWriterOpts { writerHTMLMathMethod = MathJax "Handled in the render"}
  filtered <-
    mergeDocumentMeta (setMetaValue "decker.use-data-src" False meta) pandoc
      >>= adjustResourcePathsA base
      >>= deckerMediaFilter (Disposition Page Html) base
  liftIO $ handleError $ runPure $ writeHtml5String options $ walk dropPara filtered

-- | Drops a leading Para block wrapper for a Plain wrapper.
dropPara (Para inlines) = Plain inlines
dropPara block = block

renderAnswerToHtml :: Answer -> Html
renderAnswerToHtml answer@MultipleChoice {} =
  H.ul ! A.class_ "answer multiple-choice" $ toHtml $ map render $ answer ^. answChoices
  where
    render choice =
      let solution = if choice ^. choiceCorrect then "correct" else "wrong"
       in H.li ! A.class_ solution $ do
            H.span $ if choice ^. choiceCorrect then "☑" else "☐"
            H.span $ preEscapedText $ choice ^. choiceTheAnswer
renderAnswerToHtml answer@MultipleAnswers {} =
  H.table ! A.class_ "answer multiple-answers" $ toHtml $ map render $ answer ^. answAnswers
  where
    render one =
      H.tr $ do
        H.td (preEscapedText $ one ^. oneDetail)
        H.td (preEscapedText $ one ^. oneCorrect)
renderAnswerToHtml answer@FreeForm {} = do
  let height = show (_answHeightInMm answer) :: Text
  H.p ! H.dataAttribute "height" (toValue height) $ preEscapedText $ answer ^. answCorrectAnswer
renderAnswerToHtml answer@FillText {} =
  H.p "Not yet implemented"

hn :: Int -> Html -> Html
hn 1 = H.h1
hn 2 = H.h2
hn 3 = H.h3
hn 4 = H.h4
hn 5 = H.h5
hn 6 = H.h6
hn n = throw $ InternalException $ "Haha, good one: H" <> show n

renderQuestionToHtml :: Int -> Question -> Html
renderQuestionToHtml h quest = do
  H.div ! A.class_ "question" $ do
    hn h $ do
      H.button "▶" -- ▼
      preEscapedText $ quest ^. qstTitle
    H.div ! A.class_ "closed" $ do
      H.p $ preEscapedText $ quest ^. qstQuestion
      H.p $ renderAnswerToHtml $ quest ^. qstAnswer

renderQuestionDocument :: Meta -> FilePath -> Question -> Action Text
renderQuestionDocument meta base quest = do
  htmlQuest <- compileQuestionToHtml meta base quest
  let html = renderQuestionToHtml 2 htmlQuest
  return $
    toText $
      renderHtml $
        H.html $ do
          H.head $ do
            H.meta ! A.charset "utf-8"
            H.style "img {width:100%;}"
            H.title (preEscapedText $ quest ^. qstTitle)
          H.body html

renderQuestionCatalog :: FilePath -> [Question] -> Action Text
renderQuestionCatalog base questions = do
  return $
    toText $
      renderHtml $
        H.html $ do
          H.head $ do
            H.meta ! A.charset "utf-8"
            H.title "Question Catalog"
            H.script ! A.type_ "module" ! A.src "/support/exam/catalog.js" $ ""
            H.script ! A.src "support/vendor/mathjax/tex-svg.js" $ ""
            H.link ! A.rel "stylesheet" ! A.href "/support/exam/catalog.css"
          H.body rendered
  where
    grouped :: HashMap Text (HashMap Text (NonEmpty Question))
    grouped = HashMap.map (groupBy _qstTopicId) $ groupBy _qstLectureId questions
    rendered :: Html
    rendered = toHtml $ HashMap.elems $ HashMap.mapWithKey lecture grouped
    lecture lid topics = do
      H.div
        ! A.class_ "lecture"
        ! A.id (toValue lid)
        $ do
          H.h1 $ do
            H.button "▶" -- ▼
            toHtml lid
          H.div ! A.class_ "closed" $
            toHtml $ HashMap.elems $ HashMap.mapWithKey topic topics
    topic tip quests = do
      H.div
        ! A.class_ "topic"
        ! A.id (toValue tip)
        $ do
          H.h2 $ do
            H.button "▶" -- ▼
            toHtml tip
          H.div ! A.class_ "closed" $
            toHtml $ fmap (renderQuestionToHtml 3) quests

instance ToMarkup a => ToMarkup (NonEmpty a) where
  toMarkup = toHtml . map toMarkup . toList

renderQuestion :: Meta -> FilePath -> FilePath -> Action ()
renderQuestion meta src out =
  do
    putNormal $ "# render (for " <> out <> ")"
    liftIO (readQuestion src)
      >>= renderQuestionDocument meta (takeDirectory src)
      >>= (liftIO . Text.writeFile out)

renderCatalog :: Meta -> [FilePath] -> FilePath -> Action ()
renderCatalog meta files out =
  do
    let base = takeDirectory out
    putNormal $ "# catalog (for " <> out <> ")"
    questions <- liftIO $ mapM readQuestion files
    mapM (compileQuestionToHtml meta base) questions
      >>= renderQuestionCatalog base
      >>= (liftIO . Text.writeFile out)
