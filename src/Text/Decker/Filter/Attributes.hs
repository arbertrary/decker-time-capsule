{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Text.Decker.Filter.Attributes where

import Control.Lens
import qualified Data.Map as Map
import qualified Data.Set as Set
import Relude hiding (id)
import Text.Decker.Filter.Monad
import Text.Pandoc.Definition

type Id = Text

type Classes = Set Text

type Key = Text

type Value = Text

type Attribs = Map Key Value

data ElemParams = ElemParams
  { _id :: Text,
    _cs :: Classes,
    _kvs :: Attribs,
    _sty :: Attribs,
    _qts :: Classes,
    _qps :: Attribs
  }
  deriving (Show)

makeLenses ''ElemParams

data AttribState = AttribState
  { _src :: ElemParams,
    _dst :: ElemParams
  }
  deriving (Show)

makeLenses ''AttribState

type Attrib = StateT AttribState Filter

nullElemParams = ElemParams "" Set.empty Map.empty Map.empty Set.empty Map.empty

elemParams (id, cs, kvs) = ElemParams id (Set.fromList cs) (Map.fromList kvs) Map.empty Set.empty Map.empty

runAttr :: Attr -> Attrib a -> Filter a
runAttr attr attrAction = evalStateT attrAction $ AttribState nullElemParams (elemParams attr)

class DstOp a where
  set :: a -> Attrib ()
  add :: a -> Attrib ()

class SrcOp a where
  get :: a -> Attrib a
  take :: a -> Attrib a
  drop :: a -> Attrib ()

instance DstOp Text where
  set = setId
  add = setId

instance SrcOp Text where
  get _ = getId
  take _ = takeId
  drop _ = dropId

instance DstOp Classes where
  set = setCls'
  add = addCls'

instance SrcOp Classes where
  get = getCls'
  take = takeCls'
  drop = dropCls'

instance DstOp [Text] where
  set = setCls
  add = addCls

instance SrcOp [Text] where
  get = getCls
  take = takeCls
  drop = dropCls

instance DstOp Attribs where
  set = setAttr'
  add = addAttr'

instance DstOp [(Text, Text)] where
  set = setAttr
  add = addAttr

getId :: Attrib Id
getId = use (src . id)

takeId :: Attrib Id
takeId = do
  i <- getId
  assign (src . id) ""
  return i

dropId :: Attrib ()
dropId = assign (src . id) ""

setId :: Id -> Attrib ()
setId i = assign (src . id) i

getCls' :: Classes -> Attrib Classes
getCls' want | Set.null want = use (src . cs)
getCls' want = do
  cls <- use (src . cs)
  return (Set.intersection cls want)

getCls :: [Text] -> Attrib [Text]
getCls want = toList <$> getCls' (fromList want)

takeCls' :: Classes -> Attrib Classes
takeCls' want | Set.null want = do
  cls <- use (src . cs)
  assign (src . cs) Set.empty
  return cls
takeCls' want = do
  cls <- use (src . cs)
  assign (src . cs) (Set.difference cls want)
  return (Set.intersection cls want)

takeCls :: [Text] -> Attrib [Text]
takeCls want = toList <$> takeCls' (fromList want)

dropCls' :: Classes -> Attrib ()
dropCls' want | Set.null want = do
  assign (src . cs) Set.empty
dropCls' want = do
  cls <- use (src . cs)
  assign (src . cs) (Set.difference cls want)

dropCls :: [Text] -> Attrib ()
dropCls want = dropCls' (fromList want)

setCls' :: Classes -> Attrib ()
setCls' cls = assign (dst . cs) cls

setCls :: [Text] -> Attrib ()
setCls = setCls' . fromList

addCls' :: Classes -> Attrib ()
addCls' new = do
  cls <- use (src . cs)
  assign (dst . cs) (Set.union cls new)

addCls :: [Text] -> Attrib ()
addCls = addCls' . fromList

setAttr' :: Attribs -> Attrib ()
setAttr' new = do
  assign (dst . kvs) new

addAttr' :: Attribs -> Attrib ()
addAttr' new = do
  attr <- use (dst . kvs)
  assign (dst . kvs) (Map.union new attr)

setAttr :: [(Text, Text)] -> Attrib ()
setAttr new = do
  assign (dst . kvs) (Map.fromList new)

addAttr :: [(Text, Text)] -> Attrib ()
addAttr new = do
  attr <- use (dst . kvs)
  assign (dst . kvs) (Map.union (Map.fromList new) attr)

setStyle' :: Attribs -> Attrib ()
setStyle' new = do
  assign (dst . sty) new

addStyle' :: Attribs -> Attrib ()
addStyle' new = do
  style <- use (dst . sty)
  assign (dst . sty) (Map.union new style)

setStyle :: [(Text, Text)] -> Attrib ()
setStyle new = do
  assign (dst . sty) (Map.fromList new)

addStyle :: [(Text, Text)] -> Attrib ()
addStyle new = do
  attr <- use (dst . sty)
  assign (dst . sty) (Map.union (Map.fromList new) attr)

getAttrP :: (Key -> Bool) -> Attrib Attribs
getAttrP want =
  fst . Map.partition want <$> use (src . kvs)

dropAttrP :: (Key -> Bool) -> Attrib ()
dropAttrP want = do
  (_, rest) <- Map.partition want <$> use (src . kvs)
  assign (src . kvs) rest

takeAttrP :: ((Key, Value) -> Bool) -> Attrib Attribs
takeAttrP want = do
  (match, rest) <- Map.partitionWithKey (curry want) <$> use (src . kvs)
  assign (src . kvs) rest
  return match

getAttr' :: [Key] -> Attrib Attribs
getAttr' want | null want = use (src . kvs)
getAttr' want = getAttrP (`elem` want)

getAttr :: [Key] -> Attrib [(Text, Text)]
getAttr want = Map.toList <$> getAttr' want

takeAttr' :: [Key] -> Attrib Attribs
takeAttr' want | null want = do
  attr <- use (src . kvs)
  assign (src . kvs) Map.empty
  return attr
takeAttr' want = do
  attr <- use (src . kvs)
  assign (src . kvs) (Map.filterWithKey (\k _ -> k `notElem` want) attr)
  return (Map.filterWithKey (\k _ -> k `elem` want) attr)

takeAttr :: [Key] -> Attrib [(Text, Text)]
takeAttr want = Map.toList <$> takeAttr' want

dropAttr :: [Key] -> Attrib ()
dropAttr want | null want = do
  assign (src . kvs) Map.empty
dropAttr want = do
  attr <- use (src . kvs)
  assign (src . kvs) (Map.filterWithKey (\k _ -> k `notElem` want) attr)

extract :: Attrib Attr
extract = do
  idR <- use (dst . id)
  clsR <- Set.toList <$> use (dst . cs)
  kvsR <- Map.toList <$> use (dst . kvs)
  assign (dst . id) ""
  assign (dst . cs) Set.empty
  assign (dst . kvs) Map.empty
  return (idR, clsR, kvsR)
