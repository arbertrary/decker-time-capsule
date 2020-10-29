{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Text.Decker.Filter.Quiz (
    handleQuizzes,
    Quiz (..),
    Match (..),
    Choice (..),
    QuizMeta (..),
) where

import Control.Exception
import Control.Lens hiding (Choice)
import Data.Aeson.TH
import Data.Aeson.Types
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Text.Encoding as E
import Data.Yaml hiding (YamlException)
import GHC.Generics hiding (Meta)
import Text.Blaze.Html
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Decker.Filter.Local
import Text.Decker.Internal.Common
import Text.Decker.Internal.Meta
import Text.Pandoc.Definition
import Text.Pandoc.Shared
import Text.Pandoc.Walk

-- Pair: consisting of a bucket where items should be dropped; The items which belong to the bucket
-- Distractor: Just a list of items without accompanying bucket
data Match
    = Pair {bucketID :: Int, bucket :: [Inline], items :: [[Block]]}
    | Distractor {items :: [[Block]]}
    deriving (Show)

-- | A Choice consists of a Boolean (correct), the answer text and a tooltip comment
data Choice = Choice {correct :: Bool, text :: [Inline], comment :: [Block]}
    deriving (Show)

data Difficulty
    = Easy
    | Medium
    | Hard
    | Undefined
    deriving (Eq, Show)

$(deriveJSON defaultOptions ''Difficulty)

{- | QuizFeedback is looked up in pandoc meta at the beginning of handleQuizzes
 Is not changed with the meta of a single question
-}
data QuizFeedback
    = Interactive -- Directly while navigating quiz elements
    | Immediate -- QuizFeedback for single question when pressing "check" button
    | Aggregated -- Aggregated and shown e.g. at the end of a slide on pressing "Show results". each single question has a "submit" button
    | None -- No feedback. Possibly results written to file?
    deriving (Eq, Show)

-- $(deriveJSON defaultOptions ''QuizFeedback)

-- | Set different (optional) meta options for quizzes in a yaml code block
data QuizMeta = QMeta
    { metaTopicId :: T.Text
    , metaLectureId :: T.Text
    , metaPoints :: Int
    , metaDifficulty :: Difficulty
    , metaLang :: T.Text
    , metaStyle :: T.Text
    }
    deriving (Eq, Show, Generic)

instance ToJSON QuizMeta where
    toJSON = genericToJSON $ defaultOptions{fieldLabelModifier = drop 4}
    toEncoding = genericToEncoding $ defaultOptions{fieldLabelModifier = drop 4}

instance FromJSON QuizMeta where
    parseJSON (Object q) =
        QMeta
            <$> q .:? "TopicId" .!= ""
            <*> q .:? "LectureId" .!= ""
            <*> q .:? "Points" .!= 0
            <*> q .:? "Difficulty" .!= Undefined
            <*> q .:? "lang" .!= "en"
            <*> do
                t <- q .:? "quiz"
                case t of
                    Just object -> object .:? "style" .!= ""
                    Nothing -> return ""
    parseJSON invalid = typeMismatch "QMeta" invalid

{- | The Quiz datatype.
 Each quiz type contains: title, tags/classes, meta data and the questions+answers
-}
data Quiz
    = MultipleChoice
        { -- Multiple Choice questions consist of one question (e.g. h2 header and some blocks) and a following choices/selection part
          _title :: [Inline]
        , _tags :: [T.Text]
        , _quizMeta :: QuizMeta
        , _question :: [Block]
        , _choices :: [Choice]
        }
    | MatchItems
        { -- Matching Questions consist of one question and a pairing "area" for sorting items via dragging and dropping
          _title :: [Inline]
        , _tags :: [T.Text]
        , _quizMeta :: QuizMeta
        , _question :: [Block]
        , _matchpairs :: [Match]
        }
    | InsertChoices
        { -- These questions can have multiple question and answer/choices parts.
          -- This is why questions is a list of tuples.
          _title :: [Inline]
        , _tags :: [T.Text]
        , _quizMeta :: QuizMeta
        , _questions :: [([Block], [Choice])]
        }
    | FreeText
        { _title :: [Inline]
        , _tags :: [T.Text]
        , _quizMeta :: QuizMeta
        , _question :: [Block]
        , _choices :: [Choice]
        }
    deriving (Show)

makeLenses ''Quiz

{- | Has to be called in the Markdown.hs deckerPipeline after processSlides
 | Depends on h2 headers being wrapped in boxes
-}
handleQuizzes :: Pandoc -> Decker Pandoc
handleQuizzes pandoc@(Pandoc meta blocks) =
    case quizFeedback meta of
        Aggregated -> return (Pandoc meta' (blocks' ++ aggButton))
        _ -> return p
  where
    p@(Pandoc meta' blocks') = walk parseQuizboxes pandoc
    aggButton = rawHtml' $ H.button ! A.class_ "aggButton" $ H.toHtml ("Show complete results" :: T.Text)
    parseQuizboxes :: Block -> Block
    parseQuizboxes d@(Div (id_, tgs, kvs) blocks)
        | any (`elem` tgs) ["qmi", "quiz-mi", "quiz-match-items"] =
            renderQuizzes
                meta
                (parseAndSetQuiz (setTags defaultMatch tgs) blocks)
        | any (`elem` tgs) ["qmc", "quiz-mc", "quiz-multiple-choice"] =
            renderQuizzes meta (parseAndSetQuiz (setTags defaultMC tgs) blocks)
        | any (`elem` tgs) ["qic", "quiz-ic", "quiz-insert-choices"] =
            renderQuizzes meta (parseAndSetQuiz (setTags defaultIC tgs) blocks)
        | any (`elem` tgs) ["qft", "quiz-ft", "quiz-free-text"] =
            renderQuizzes meta (parseAndSetQuiz (setTags defaultFree tgs) blocks)
        | otherwise = d
    parseQuizboxes bl = bl
    -- Give the tag-/classlist of the surrounding div box to the quiz
    setTags :: Quiz -> [T.Text] -> Quiz
    setTags q ts =
        if elem "columns" ts
            then set tags ts q
            else set tags (ts ++ ["columns", "box"]) q
    -- The default "new" quizzes
    defaultQMeta =
        QMeta
            { metaTopicId = ""
            , metaLectureId = ""
            , metaPoints = 0
            , metaDifficulty = Undefined
            , metaLang = (lookupMetaOrElse "en" "lang" meta)
            , metaStyle = (lookupMetaOrElse "plain" "quiz.style" meta)
            }
    defaultMatch = MatchItems [] [] defaultQMeta [] []
    defaultMC = MultipleChoice [] [] defaultQMeta [] []
    defaultIC = InsertChoices [] [] defaultQMeta []
    defaultFree = FreeText [] [] defaultQMeta [] []

quizFeedback :: Meta -> QuizFeedback
quizFeedback meta = case lookupMetaOrElse ("Interactive" :: T.Text) "quiz.feedback" meta of
    "Interactive" -> Interactive
    "Immediate" -> Immediate
    "Aggregated" -> Aggregated
    "None" -> None
    _ -> Interactive

-- Take the parsed Quizzes and render them to html
renderQuizzes :: Meta -> Quiz -> Block
renderQuizzes meta quiz@MatchItems{} = renderMatching meta quiz
renderQuizzes meta quiz@FreeText{} = renderFreeText meta quiz
renderQuizzes meta quiz@InsertChoices{} = renderInsertChoices meta quiz
renderQuizzes meta quiz@MultipleChoice{} = renderMultipleChoice meta quiz

-- Takes a Quiz and a list of blocks, parses the blocks and modifies the given quiz
parseAndSetQuiz :: Quiz -> [Block] -> Quiz
parseAndSetQuiz q bs = combineICQuestions (foldl parseAndSetQuizFields q bs)

{- | This function combines the (Question, Choices) tuples of InsertChoice questions
 These tuples are of type ([Block], [Choice])
 This is because in `parseAndSetQuizFields` they are created alternatingly as (bs, []) and ([],chs)
 This combine function makes it so that the questions field in InsertChoice is
 an alternating list of "Question text -> quiz element -> question text" etc
-}
combineICQuestions :: Quiz -> Quiz
combineICQuestions quiz@(InsertChoices ti tgs qm q) = set questions (combineQTuples q) quiz
  where
    -- These tuples can only be ([],a) or (a,[]) per default
    -- They're created like this in parseAndSetQuizFields
    combineQTuples ::
        [([Block], [Choice])] -> [([Block], [Choice])]
    combineQTuples [] = []
    combineQTuples bc@[(a, b)] = bc
    -- combine two question blocks
    combineQTuples ((a, []) : (b, []) : rest) =
        combineQTuples ((a ++ b, []) : rest)
    -- combine Question with a choice block
    combineQTuples ((a, []) : ([], b) : rest) =
        (a, b) : combineQTuples rest
    -- If the head has anything other than an empty choice then ignore it
    combineQTuples (a : (y, b) : rest) = a : combineQTuples ((y, b) : rest)
combineICQuestions q = q

readQuizMeta :: T.Text -> QuizMeta
readQuizMeta text = do
    let result = decodeEither' (encodeUtf8 text)
    case result of
        Right meta -> meta
        Left exception -> QMeta "ERROR" "ERROR" 0 Easy "ERROR" "ERROR"

-- | This monolithic function parses a Pandoc Block and uses lenses to set the field in the given quiz item
parseAndSetQuizFields :: Quiz -> Block -> Quiz
-- Set the title
parseAndSetQuizFields q (Header 2 (id_, cls, kvs) text) = set title text q
-- Set the meta information
parseAndSetQuizFields quiz (CodeBlock (id_, cls, kvs) code) =
    if "yaml" `elem` cls
        then set quizMeta (readQuizMeta code) quiz
        else quiz
-- Set quiz pairs/Match Items
-- Zip with index
parseAndSetQuizFields quiz@MatchItems{} (DefinitionList items) =
    set matchpairs (map parseDL (zip [1 ..] items)) quiz
  where
    parseDL :: (Int, ([Inline], [[Block]])) -> Match
    parseDL (i, (Str "!" : _, bs)) = Distractor bs
    parseDL (i, (is, [Plain (Str "!" : inl)] : bs)) =
        Pair i is [[Plain []]]
    parseDL (i, (is, bs)) = Pair i is bs
-- parse and set choices for FreeText
parseAndSetQuizFields quiz@FreeText{} (BulletList blocks) =
    set choices (map (parseQuizTLItem quiz) blocks) quiz
-- parse and Set choices for InsertChoices
parseAndSetQuizFields quiz@(InsertChoices ti tgs qm q) (BulletList blocks) =
    set questions (q ++ [([], map (parseQuizTLItem quiz) blocks)]) quiz
-- Parse and set choices for MultipleChoice
parseAndSetQuizFields quiz@MultipleChoice{} (BulletList blocks) =
    set choices (map (parseQuizTLItem quiz) blocks) quiz
-- The questions are prepended
-- Parse and set question for matching
parseAndSetQuizFields quiz@(MatchItems ti tgs qm q p) b =
    set question (q ++ [b]) quiz
-- Parse and set questions for FreeText
parseAndSetQuizFields quiz@(FreeText ti tgs qm q ch) b =
    set question (q ++ [b]) quiz
-- Set question for InsertChoices
parseAndSetQuizFields quiz@(InsertChoices ti tgs qm q) b =
    set questions (q ++ [([b], [])]) quiz
-- Set question for Multiple Choice
parseAndSetQuizFields quiz@(MultipleChoice ti tgs qm q ch) b =
    set question (q ++ [b]) quiz

{- | Parse a Pandoc Bullet/Task list item to a Choice
 Pandoc replaces [X] internally with unicode checkboxes before quiz parsing
 This is why we pattern match for "☒" and "☐" here
-}
parseQuizTLItem :: Quiz -> [Block] -> Choice
parseQuizTLItem _ (Plain (Str "☒" : Space : is) : bs) = Choice True is bs
parseQuizTLItem _ (Plain (Str "☐" : Space : is) : bs) = Choice False is bs
parseQuizTLItem FreeText{} (Plain is : bs) = Choice True is bs
-- parseQuizTLItem InsertChoices {} (Plain is:bs) = Choice True is bs
parseQuizTLItem _ is =
    Choice False [Str "Error: NoTasklistItem"] [Plain []]

-- | A simple Html button
solutionButton :: Meta -> Block
solutionButton meta =
    rawHtml' $ do H.button ! A.class_ "solutionButton" $ H.toHtml buttonText
  where
    buttonText :: T.Text
    buttonText = lookupInDictionary "quiz.solution" meta

resetButton :: Meta -> Block
resetButton meta =
    rawHtml' $ do H.button ! A.class_ "resetButton" $ H.toHtml buttonText
  where
    buttonText :: T.Text
    buttonText = lookupInDictionary "quiz.reset-button" meta

submitButton :: Block
submitButton = rawHtml' $ H.button ! A.class_ "submitButton" $ H.toHtml ("Submit Solution" :: T.Text)

quizAttributes :: QuizMeta -> [(T.Text, T.Text)]
quizAttributes quizMeta =
    [ ("data-points", T.pack $ show $ metaPoints quizMeta)
    , ("data-difficulty", T.pack $ show $ metaDifficulty quizMeta)
    , ("data-topic-id", metaTopicId quizMeta)
    , ("data-lecture-id", metaLectureId quizMeta)
    ]

quizStyle :: Meta -> QuizMeta -> T.Text
quizStyle pandocMeta quizMeta = case metaStyle quizMeta of
    "" -> lookupMetaOrElse "plain" "quiz.style" pandocMeta
    s -> s

renderMultipleChoice :: Meta -> Quiz -> Block
renderMultipleChoice pandocMeta quiz@(MultipleChoice title tgs qm q ch) =
    Div ("", cls, quizAttributes qm) $ header ++ q ++ [choiceBlock, Plain [LineBreak]] ++ button
  where
    button = case quizFeedback pandocMeta of
        Immediate -> [submitButton]
        Aggregated -> [submitButton]
        _ -> []
    cls = tgs ++ [quizStyle pandocMeta qm] ++ [T.pack $ show $ quizFeedback pandocMeta]
    header =
        case title of
            [] -> []
            _ -> [Header 2 ("", [], []) title]
    choiceBlock = rawHtml' $ choiceList "choices" ch
renderMultipleChoice meta q =
    Div ("", [], []) [Para [Str "ERROR NO MULTIPLE CHOICE QUIZ"]]

{- | Transform a list of Choices to an HTML <ul>
 This is used directly in multiple choice questions and as "solutionList" in IC and FT questions
-}
choiceList :: AttributeValue -> [Choice] -> Html
choiceList t choices =
    H.ul ! A.class_ t $
        foldr ((>>) . handleChoices) (H.span $ H.toHtml ("" :: T.Text)) choices
  where
    reduceTooltip :: [Block] -> [Block]
    reduceTooltip [BulletList blocks] -- = concat blocks
        =
        concatMap (\x -> x ++ [Plain [LineBreak]]) blocks
    reduceTooltip bs = bs
    handleChoices :: Choice -> Html
    handleChoices (Choice correct text comment) =
        if correct
            then H.li ! A.class_ "correct" $
                do
                    toHtml text
                    H.div ! A.class_ "tooltip" $ toHtml (reduceTooltip comment)
            else H.li ! A.class_ "wrong" $
                do
                    toHtml text
                    H.div ! A.class_ "tooltip" $ toHtml (reduceTooltip comment)

renderInsertChoices :: Meta -> Quiz -> Block
renderInsertChoices pandocMeta quiz@(InsertChoices title tgs qm q) =
    Div ("", cls, quizAttributes qm) $ header ++ questionBlocks q ++ tooltipDiv
  where
    cls = tgs ++ [quizStyle pandocMeta qm] ++ [T.pack $ show $ quizFeedback pandocMeta]
    -- ++ [view solution qm]
    -- ++ [solutionButton]
    header =
        case title of
            [] -> []
            _ -> [Header 2 ("", [], []) title]
    tooltipDiv = [Div ("", [T.pack "tooltip-div"], []) []]
    questionBlocks :: [([Block], [Choice])] -> [Block]
    questionBlocks = map (rawHtml' . handleTuple)
    handleTuple :: ([Block], [Choice]) -> Html
    handleTuple ([], chs) = select chs
    handleTuple (bs, []) = toHtml (map reduceBlock bs)
    handleTuple (bs, chs) = toHtml (map reduceBlock bs) >> select chs
    reduceBlock :: Block -> Block
    reduceBlock (Para is) = Plain ([Str " "] ++ is ++ [Str " "])
    reduceBlock p = p
    select :: [Choice] -> Html
    select choices =
        ( H.select $
            (H.option ! A.class_ "wrong" $ H.toHtml ("..." :: T.Text))
                >> (foldr ((>>) . options) H.br choices)
        )
            >> choiceList "solutionList" choices
    options :: Choice -> Html
    options (Choice correct text comment) =
        if correct
            then H.option ! A.class_ "correct" ! value $ toHtml $ stringify text
            else H.option ! A.class_ "wrong" ! value $ toHtml $ stringify text
      where
        value = A.value $ textValue $ stringify text
renderInsertChoices meta q =
    Div ("", [], []) [Para [Str "ERROR NO INSERT CHOICES QUIZ"]]

-- | Creates a custom drop-down select element that allows to select multiple options
buildSelect :: [[Block]] -> Block
buildSelect items = Div ("", ["options"], []) $ [blank] ++ optList
  where
    blank :: Block
    blank = rawHtml' (H.p ! A.class_ "option" $ "...")
    optList = map createOptions (zip [0 ..] (concat items))
    createOptions :: (Int, Block) -> Block
    createOptions (i, item@(Div (t, ["matchItem"], [("draggable", "true"), ("bucketId", bID)]) bs)) =
        rawHtml' (H.p ! A.class_ "option" ! H.customAttribute "data-bucketId" (H.textValue bID) $ H.toHtml ([toEnum (i + 65), '.'] :: String))
    createOptions (i, bs) = rawHtml' (H.p ! A.class_ "option" $ H.toHtml ([toEnum (i + 65), '.'] :: String))

{- | Creates matchQuestion divs for plain matching
 Each matchQuestion consists of the bucket-label and a select-dropdown element created in buildSelect
-}
plainMatchQuestionsDivs :: [Block] -> [[Block]] -> [Block]
plainMatchQuestionsDivs buckets items = map matchQuestionDiv (filter (not . isDistractor) buckets)
  where
    matchQuestionDiv bucket = Div ("", ["matchQuestion"], []) $ [label bucket, optList, options]
    isDistractor :: Block -> Bool
    isDistractor Text.Pandoc.Definition.Null = True
    isDistractor _ = False
    label :: Block -> Block
    label bucket@(Div ("", ["bucket"], [("bucketId", bID)]) bs) = rawHtml' $ H.label ! H.customAttribute "data-bucketId" (H.textValue bID) $ toHtml (stringify bucket)
    label bucket = rawHtml' $ H.label $ toHtml (stringify bucket)
    optList :: Block
    optList = Div ("", ["optList"], []) [rawHtml' (H.p ! A.class_ "selected blank option" $ "...")]
    options = buildSelect items

{- | Creates the HTML elements for Matching Questions
 Changes depending on if the style is "plain" or something else
-}
renderMatching :: Meta -> Quiz -> Block
renderMatching pandocMeta quiz@(MatchItems title tgs qm qs matches) =
    case quizStyle pandocMeta qm of
        "plain" -> Div ("", cls, quizAttributes qm) $ header ++ qs ++ [plainMatchDiv, sButton]
        _ -> Div ("", cls, quizAttributes qm) $ header ++ qs ++ [itemsDiv, bucketsDiv, sButton]
  where
    cls = tgs ++ [quizStyle pandocMeta qm] ++ [T.pack $ show $ quizFeedback pandocMeta]
    newMeta = setMetaValue "lang" (metaLang qm) pandocMeta
    sButton = solutionButton newMeta
    header =
        case title of
            [] -> []
            _ -> [Header 2 ("", [], []) title]
    (buckets, items) = unzip $ map pairs matches
    dropHint = ("data-hint", lookupInDictionary "quiz.qmi-drop-hint" newMeta)
    dragHint = ("data-hint", lookupInDictionary "quiz.qmi-drag-hint" newMeta)
    --
    itemsDiv = Div ("", ["matchItems"], [dragHint]) (concat items)
    bucketsDiv = Div ("", ["buckets"], [dropHint]) buckets
    -- The divs needed for the plain matching style
    plainMatchDiv = Div ("", ["matchDiv"], []) [plainItemsDiv, plainBucketsDiv]
    plainItemsDiv = Div ("", ["matchItems"], []) (plainMatchQuestionsDivs buckets items)
    plainBucketsDiv = Div ("", ["buckets"], []) (concat items)
    item :: T.Text -> [Block] -> Block
    item index =
        Div
            ( ""
            , ["matchItem"]
            , [("draggable", "true"), ("bucketId", index)]
            )
    distractor :: [Block] -> Block
    distractor =
        Div ("", ["matchItem", "distractor"], [("draggable", "true")])
    pairs :: Match -> (Block, [Block])
    pairs (Distractor bs) = (Text.Pandoc.Definition.Null, map distractor bs)
    pairs (Pair i is bs) =
        case bs of
            [[Plain []]] ->
                ( Div
                    ( ""
                    , ["bucket", "distractor"]
                    , [("bucketId", T.pack $ show i)]
                    )
                    [Plain is]
                , []
                )
            _ ->
                ( Div
                    ("", ["bucket"], [("bucketId", T.pack $ show i)])
                    [Plain is]
                , map (item (T.pack $ show i)) bs
                )
renderMatching meta q =
    Div ("", [], []) [Para [Str "ERROR NO MATCHING QUIZ"]]

renderFreeText :: Meta -> Quiz -> Block
renderFreeText pandocMeta quiz@(FreeText title tgs qm q ch) =
    Div ("", cls, quizAttributes qm) $ header ++ q ++ [inputRaw] ++ [sButton] ++ [rButton] ++ [sol]
  where
    cls = tgs ++ [quizStyle pandocMeta qm] ++ [T.pack $ show $ quizFeedback pandocMeta]
    newMeta = setMetaValue "lang" (metaLang qm) pandocMeta
    sButton = solutionButton newMeta
    rButton = resetButton newMeta
    header =
        case title of
            [] -> []
            _ -> [Header 2 ("", [], []) title]
    placeholderText :: T.Text
    placeholderText = lookupInDictionary "quiz.input-placeholder" newMeta
    inputRaw =
        rawHtml'
            ( (H.input ! A.placeholder (H.textValue placeholderText))
                >> choiceList "solutionList" ch
            )
    sol = rawHtml' $ H.ul ! A.class_ "solutionDiv" $ ""
renderFreeText meta q =
    Div ("", [], []) [Para [Str "ERROR NO FREETEXT QUIZ"]]
