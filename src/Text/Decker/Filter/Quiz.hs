{-| 
  Module: Quiz
  Description: Provides functionality for creating different types of quiz questions
  Author: Jan-Philipp Stauffert <jan-philipp.stauffert@uni-wuerzburg.de> 
  Author: Armin Bernstetter <armin.bernstetter@uni-wuerzburg.de>
  Author: Samantha Monty <samantha.monty@uni-wuerzburg.de>
  
  This module enables creating different types of quiz questions in decker.
  Currently possible: 
    - Blanktext/Cloze tests
    - Multiple choice
    - Free text questions 
    - Matching/pair questions
-}
module Text.Decker.Filter.Quiz
  ( renderQuizzes
  ) where

import Control.Lens ((^.))
import Control.Monad.IO.Class
import Data.List
import Data.List.Split
import Text.Decker.Filter.Util
import Text.Decker.Internal.Common
import Text.Decker.Internal.Meta
import Text.Decker.Project.Project
import Text.Pandoc
import Text.Pandoc.Shared
import Text.Pandoc.Walk
import Text.Printf

scormQuiz :: Meta -> IO Bool
scormQuiz meta =
  case getMetaBool "scorm" meta of
    Just _ -> return True
    Nothing -> return False

-- | Render all types of questions
renderQuizzes :: Pandoc -> Decker Pandoc
renderQuizzes pandoc = do
  dirs <- liftIO projectDirectories
  meta <- liftIO $ readMetaData $ dirs ^. project
  isScorm <- liftIO $ scormQuiz meta
  let mc =
        if isScorm
          then walk renderScormMC $ addInstructions pandoc meta
          else walk renderMultipleChoice pandoc
  let match = walk renderMatching mc
  let blank = walk renderBlanktext match
  return $ walk renderFreetextQuestion blank

-- A multiple choice question is a bullet list in the style of a task list.
-- A div class survey is created around the bullet list
renderMultipleChoice :: Block -> Block
renderMultipleChoice (BulletList blocks@((firstBlock:_):_))
  | checkIfMC firstBlock =
    Div
      ("", ["survey"], [])
      [BulletList (map multipleChoiceHtml blocks), answerButton]
  where
    answerButton =
      Para $
      [LineBreak] ++
      [toHtml "<button class=\"mcAnswerButton\" type=\"button\">"] ++
      [Str "Show Solution"] ++ [toHtml "</button>"]
-- Default pass through
renderMultipleChoice block = block

-- Add a single slide with quiz instructions to the front of the slide deck
addInstructions :: Pandoc -> Meta -> Pandoc
addInstructions pandoc@(Pandoc meta blocks) metadata =
  case getMetaString "title" meta of
    Just "Generated Index" -> pandoc
    _ ->
      Pandoc
        meta
        (Header 1 ("instructions", [], []) [Str "Quiz Instructions"] :
         Para [text] : blocks)
    Nothing -> pandoc
  where
    select =
      "You may select more than one response per question. You will receive 1 point for each correct response. "
    lose = "You will lose 1 point for each incorrect response. "
    allCorrect =
      "It is possible that all responses are correct or all responses are incorrect. "
    one = "Please select only one response per question. "
    gradingScheme =
      case getMetaString "grading-scheme" metadata of
        Just "BV1" -> select ++ lose ++ allCorrect
        Just "BV2" -> select ++ allCorrect
        Just "BV3" -> select ++ allCorrect
        _ -> one
    text =
      Str $
      "This is a multiple-choice quiz. " ++
      gradingScheme ++
      "At the end of the quiz, click the Submit All button to submit your responses. " ++
      "You may change a response at any time before submitting."

-- | Renders a multiple choice question for SCORM
renderScormMC :: Block -> Block
renderScormMC (BulletList blocks@((firstBlock:_):_))
  | checkIfMC firstBlock =
    Div ("", ["scorm-survey"], []) [BulletList (map multipleChoiceHtml blocks)]
renderScormMC block = block

-- | Renders a freetext question from a bullet list with special syntax
renderFreetextQuestion :: Block -> Block
renderFreetextQuestion bl@(BulletList ((firstBlock:_):(sndBlock:_):_)) =
  case (checkIfFreetextQuestion firstBlock, checkIfFreetextAnswer sndBlock) of
    (Just q, Just a) ->
      Div
        ("", ["freetextQuestion"], [])
        [Para $ freetextQuestionHtml q (stringify a)]
    _ -> bl
renderFreetextQuestion block = block

-- | Renders a "matching" question from a definition list with special syntax
renderMatching :: Block -> Block
renderMatching dl@(DefinitionList items) =
  case traverse checkIfMatching items of
    Just l -> matchingHtml l
    Nothing -> dl
renderMatching block = block

-- | Renders a "blanktext" Cloze question from a definition list with special syntax
renderBlanktext :: Block -> Block
renderBlanktext dl@(DefinitionList items) =
  case traverse checkIfBlanktext items of
    Just l -> Div ("", [], []) (map blanktextHtml l)
    Nothing -> dl
renderBlanktext block = block

{-
Functions that create lower level Html Elements for the question types using Pandocs Block/Inline Data types
-}
-- | Renders a multiple choice answer 
-- Throws away the identifier and sourrounds the content with a div
-- The div has the class right or wrong according to how it was marked
multipleChoiceHtml :: [Block] -> [Block]
multipleChoiceHtml (prelude:rest) =
  [Div ("", "answer" : cls, []) (prelude' : (map mcTooltipHtml rest))]
  where
    (cls, prelude') =
      case prelude of
        Para ((Str "{X}"):prest) -> (["lft"], Para prest)
        Para ((Str "{"):Space:(Str "}"):prest) -> (["rgh"], Para prest)
        Plain ((Str "{X}"):prest) -> (["lft"], Para prest)
        Plain ((Str "{"):Space:(Str "}"):prest) -> (["rgh"], Para prest)
        prest -> ([], prest)

-- if there is a bullet list create a div class tooltip around
-- if there are multiple bullet points, all but the first are thrown away
mcTooltipHtml :: Block -> Block
mcTooltipHtml (BulletList (content:_)) = Div ("", ["tooltip"], []) content
mcTooltipHtml block = block

-- | create the html element for the blanktext question
blanktextHtml :: ([Inline], [Block]) -> Block
blanktextHtml (inlines, blocks) =
  Div ("", ["blankText"], []) ([title] ++ selects ++ [answerButton])
  where
    title = Header 2 ("", [], []) inlines
    selects = map html blocks
    html (Plain x) = Para (blanktextHtmlAnswers $ splitBlankText x)
    answerButton =
      Para $
      [toHtml "<button class=\"btAnswerButton\" type=\"button\">"] ++
      [Str "Show Solution"] ++ [toHtml "</button>"]
    -- | Split the Blanktext Inline into a List of strings. 
    -- The list elements are either simple text or a String of answer options that gets processed later
    splitBlankText :: [Inline] -> [String]
    splitBlankText inlines =
      concatMap
        (split (startsWith "{"))
        (split (endsWith "}") (stringify inlines))

-- | Takes the List of Strings (text + possible answer options) and if it's an answer list generate a dropdown menu
blanktextHtmlAnswers :: [String] -> [Inline]
blanktextHtmlAnswers =
  concatMap
    (\x
      -- If the answers contain only one element without separators
      -- Create an HTML input field
      ->
       if "{" `isPrefixOf` x && "}" `isSuffixOf` x && not ("|" `isInfixOf` x)
         then [ toHtml
                  (printf
                     "<input type=\"text\" answer=\"%s\" class=\"blankInput\">"
                     (filter (/= '!') . drop 1 . init $ x))
              ]
          -- else the answers contain multiple elements separated by "|"
          -- Create an HTML select element
         else if "{" `isPrefixOf` x && "}" `isSuffixOf` x && "|" `isInfixOf` x
                then [toHtml "<select class=\"blankSelect\">"] ++
                     map insertOption (split' x) ++ [toHtml "</select>"]
                -- Else the string is filler text
                else [Str x])
  where
    split' = splitOn "|" . drop 1 . init
    -- Take an answer option and create an HTML option element. 
    -- If its prefix is "!" it's a correct answer
    insertOption :: String -> Inline
    insertOption ('!':x) =
      toHtml
        (printf
           "<option class=\"blankOption\" answer=\"true\" value=\"%s\">%s</option>"
           x
           x)
    insertOption x =
      toHtml
        (printf
           "<option class=\"blankOption\" answer=\"false\" value=\"%s\">%s</option>"
           x
           x)

-- | Creates the html representation for a matching question
matchingHtml :: [([Inline], [[Block]])] -> Block
matchingHtml dListItems =
  Div ("", ["matching"], []) [dropzones, dragzone, answerButton]
  where
    (inlines, blocks) = unzip dListItems
    draggable = Div ("", ["draggable"], [("draggable", "true")])
    dragzone = Div ("", ["dragzone"], []) (fmap draggable (concat blocks))
    dropzones = wrapDrop inlines
    answerButton =
      Para $
      [LineBreak] ++
      [toHtml "<button class=\"matchingAnswerButton\" type=\"button\">"] ++
      [Str "Show Solution"] ++
      [toHtml "</button>"] ++
      [toHtml "<button class=\"retryButton\" type=\"button\">"] ++
      [Str "Retry"] ++ [toHtml "</button>"]
    wrapDrop :: [[Inline]] -> Block
    wrapDrop inlines = Div ("", ["dropzones"], []) dropzones
      where
        dropzones = (\i -> Div ("", ["dropzone"], []) [Plain i]) <$> inlines

-- 
freetextQuestionHtml :: [Inline] -> String -> [Inline]
freetextQuestionHtml question answer =
  [toHtml "<form onSubmit=\"return false;\">"] ++
  question ++
  [LineBreak] ++
  [ toHtml
      ("<input type=\"text\" answer=\"" ++
       answer ++ "\" class=\"freetextInput\">")
  ] ++
  -- 
  [LineBreak] ++
  [toHtml "<button class=\"freetextAnswerButton\" type=\"button\">"] ++
  [Str "Show Solution"] ++ [toHtml "</button>"] ++ [toHtml "</form>"]

-- | Check if a DefinitionList is a blank text question
checkIfBlanktext :: ([Inline], [[Block]]) -> Maybe ([Inline], [Block])
checkIfBlanktext ([Str "{blanktext}"], firstBlock:_) = Just ([], firstBlock)
checkIfBlanktext (Str "{blanktext}":Space:rest, firstBlock:_) =
  Just (rest, firstBlock)
checkIfBlanktext _ = Nothing

-- | Check if a DefinitionList is a matching question
checkIfMatching :: ([Inline], [[Block]]) -> Maybe ([Inline], [[Block]])
checkIfMatching (Str "{match}":Space:rest, firstBlock:_) =
  Just (rest, [firstBlock])
checkIfMatching _ = Nothing

checkIfFreetextQuestion :: Block -> Maybe [Inline]
checkIfFreetextQuestion (Para (Str "{?}":q)) = Just q
checkIfFreetextQuestion (Plain (Str "{?}":q)) = Just q
checkIfFreetextQuestion _ = Nothing

checkIfFreetextAnswer :: Block -> Maybe [Inline]
checkIfFreetextAnswer (Para (Str "{!}":a)) = Just a
checkIfFreetextAnswer (Plain (Str "{!}":a)) = Just a
checkIfFreetextAnswer _ = Nothing

-- | Checks if a block starts with [X] or [ ] to indicate a survey
checkIfMC :: Block -> Bool
checkIfMC (Para ((Str "{X}"):_)) = True
checkIfMC (Para ((Str "{"):Space:(Str "}"):_)) = True
checkIfMC (Plain ((Str "{X}"):_)) = True
checkIfMC (Plain ((Str "{"):Space:(Str "}"):_)) = True
checkIfMC _ = False
