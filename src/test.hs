{-# LANGUAGE TemplateHaskell #-}

module Test (Question(..), Answer(..), Difficulty(..), Templates, compileTesterTemplates, selectTemplate) where

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

data Question =
  Question {qstId :: String
           ,qstLecture :: Int
           ,qstTitle :: String
           ,qstPoints :: Int
           ,qstQuestion :: String
           ,qstAnswer :: Answer
           ,qstDifficulty :: Difficulty
           ,qstComment :: String}
  deriving (Show,Typeable)

data Answer
  = MultipleChoice {answCorrect :: [String]
                   ,answIncorrect :: [String]}
  | FillText {answFillText :: String
             ,answCorrectWords :: [String]}
  | FreeForm {answHeightInMm :: Int,
          answCorrectAnswer :: String}
  deriving (Show,Typeable)

data Difficulty = Easy | Medium | Hard
  deriving (Show,Typeable)

$(deriveJSON defaultOptions{fieldLabelModifier = drop 4} ''Answer)

$(deriveJSON defaultOptions{fieldLabelModifier = drop 3} ''Question)

$(deriveJSON defaultOptions ''Difficulty)

mcKey = typeOf $ MultipleChoice [] []
ftKey = typeOf $ FillText "" []
ffKey = typeOf $ FreeForm 0 ""

type Templates = [(TypeRep,M.Template)]

selectTemplate
  :: Templates -> Question -> M.Template
-- selectTemplate templates question = fromJust $ lookup (typeOf $ qstAnswer question) templates
selectTemplate templates question = case qstAnswer question of
  MultipleChoice _ _ -> compileMustacheTemplate testerMultipleChoiceTemplate
  FillText _ _ -> compileMustacheTemplate testerFillTextTemplate
  FreeForm _ _ -> compileMustacheTemplate testerFreeFormTemplate

compileTesterTemplates :: Templates
compileTesterTemplates =
  [(mcKey, compileMustacheTemplate testerMultipleChoiceTemplate)
  ,(ftKey, compileMustacheTemplate testerFillTextTemplate)
  ,(ffKey, compileMustacheTemplate testerFreeFormTemplate)]

compileMustacheTemplate :: B.ByteString -> M.Template
compileMustacheTemplate string =
  case (M.compileTemplate "" . E.decodeUtf8) string of
    Left err -> throw $ MustacheException $ show err
    Right template -> template
