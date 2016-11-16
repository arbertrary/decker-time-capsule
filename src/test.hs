{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module Test
       (Question(..), Answer(..), Difficulty(..), Exam(..), Templates,
        compileTesterTemplates, selectTemplate)
       where

import Control.Exception
import Data.Yaml
import Data.Aeson.Types
import Data.Char
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.ByteString.Char8 as B
import Data.Typeable
import qualified Text.Mustache as M
import qualified Text.Mustache.Types as MT
import Embed
import Utilities
import Data.Aeson.TH

data Question = Question
    { qstTopicId :: T.Text
    , qstLectureId :: T.Text
    , qstTitle :: T.Text
    , qstPoints :: Int
    , qstQuestion :: T.Text
    , qstAnswer :: Answer
    , qstDifficulty :: Difficulty
    , qstComment :: T.Text
    } deriving (Eq,Show,Typeable)

data Answer
    = MultipleChoice { answCorrect :: [T.Text]
                     , answIncorrect :: [T.Text]}
    | FillText { answFillText :: T.Text
               , answCorrectWords :: [T.Text]}
    | FreeForm { answHeightInMm :: Int
               , answCorrectAnswer :: T.Text}
    deriving (Eq,Show,Typeable)

data Difficulty
    = Easy 
    | Medium 
    | Hard 
    deriving (Eq,Show,Typeable)

data Exam = Exam
    { examStudentInfoFile :: FilePath
    , examDateTime :: T.Text
    , examDurationInMinutes :: T.Text
    , examTrack :: Int
    , examLectureIds :: [T.Text]
    , examExcludedTopicIds :: [T.Text]
    } deriving (Eq,Show,Typeable)

$(deriveJSON
      defaultOptions
      { fieldLabelModifier = drop 4
      }
      ''Answer)

$(deriveJSON
      defaultOptions
      { fieldLabelModifier = drop 3
      }
      ''Question)

$(deriveJSON
      defaultOptions
      { fieldLabelModifier = drop 4
      }
      ''Exam)

$(deriveJSON defaultOptions ''Difficulty)

mcKey = typeOf $ MultipleChoice [] []

ftKey = typeOf $ FillText "" []

ffKey = typeOf $ FreeForm 0 ""

type Templates = [(TypeRep, M.Template)]

selectTemplate :: Templates -> Question -> M.Template
-- selectTemplate templates question = fromJust $ lookup (typeOf $ qstAnswer question) templates
selectTemplate templates question = 
    case qstAnswer question of
        MultipleChoice _ _ -> 
            compileMustacheTemplate $ fixMustacheMarkup testerMultipleChoiceTemplate
        FillText _ _ -> compileMustacheTemplate $ fixMustacheMarkup testerFillTextTemplate
        FreeForm _ _ -> compileMustacheTemplate $ fixMustacheMarkup testerFreeFormTemplate

compileTesterTemplates :: Templates
compileTesterTemplates = 
    [ (mcKey, compileMustacheTemplate $ fixMustacheMarkup testerMultipleChoiceTemplate)
    , (ftKey, compileMustacheTemplate $ fixMustacheMarkup testerFillTextTemplate)
    , (ffKey, compileMustacheTemplate $ fixMustacheMarkup testerFreeFormTemplate)]

compileMustacheTemplate :: T.Text -> M.Template
compileMustacheTemplate string = 
    case M.compileTemplate "" string of
        Left err -> throw $ MustacheException $ show err
        Right template -> template