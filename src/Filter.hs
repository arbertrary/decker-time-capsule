{-- Author: Henrik Tramberend <henrik@tramberend.de> --}
{-# LANGUAGE OverloadedStrings #-}

module Filter
  ( Disposition(..)
  , expandMacros
  , makeSlides
  , filterNotes
  , makeBoxes
  , useCachedImages
  , escapeToFilePath
  , cachePandocImages
  , extractLocalImagePathes
  , renderMediaTags
  , transformImageSize
  , lazyLoadImage
  , isMacro
  , iframeExtensions
  , audioExtensions
  , videoExtensions
  ) where

import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Default ()
import Data.List
import Data.List.Split
import qualified Data.Map as Map (Map, fromList, lookup)
import Data.Maybe
import qualified Network.URI as U

-- import qualified Data.Set as Set
-- import Debug.Trace
import Network.HTTP.Conduit
import Network.HTTP.Simple
import Network.URI (parseURI, uriScheme)
import System.Directory
import System.FilePath

-- import System.FilePath.Posix
import Text.Blaze (customAttribute)
import Text.Blaze.Html.Renderer.String
import Text.Blaze.Html5 as H
       ((!), audio, div, figure, iframe, img, p, stringTag, toValue,
        video, iframe)
import Text.Blaze.Html5.Attributes as A
       (alt, class_, height, id, src, style, title, width)
import Text.Pandoc
import Text.Pandoc.Definition ()

-- import Text.Pandoc.JSON
import Text.Pandoc.Shared
import Text.Pandoc.Walk
import Text.Printf
import Text.Read

type MacroFunc = [String] -> Attr -> Target -> Format -> Meta -> Inline

-- iframe resizing, see:
-- https://css-tricks.com/NetMag/FluidWidthVideo/Article-FluidWidthVideo.php
-- YouTube links: iv_load_policy=3 disables annotations, rel=0 disables related
-- videos. See:
-- https://developers.google.com/youtube/player_parameters?hl=de#IFrame_Player_API
embedYoutubeHtml :: [String] -> Attr -> Target -> Inline
embedYoutubeHtml args attr (vid, _) =
  RawInline (Format "html") (renderHtml html)
  where
    url =
      printf
        "https://www.youtube.com/embed/%s?iv_load_policy=3&disablekb=1&rel=0&modestbranding=1&autohide=1"
        vid :: String
    vidWidthStr = macroArg 0 args "560"
    vidHeightStr = macroArg 1 args "315"
    vidWidth = readDefault 560.0 vidWidthStr :: Float
    vidHeight = readDefault 315.0 vidHeightStr :: Float
    wrapperStyle =
      printf
        "position:relative;padding-top:25px;padding-bottom:%f%%;height:0;"
        (vidHeight / vidWidth * 100.0) :: String
    iframeStyle =
      "position:absolute;top:0;left:0;width:100%;height:100%;" :: String
    figureStyle (_, _, kv) =
      foldl (\s (k, v) -> s ++ printf "%s:%s;" k v :: String) "" kv
    figureClass (_, cls, _) = unwords cls
    html =
      H.figure ! class_ (toValue (figureClass attr)) !
      style (toValue (figureStyle attr)) $
      H.div ! style (toValue wrapperStyle) $
      iframe ! style (toValue iframeStyle) ! width (toValue vidWidthStr) !
      height (toValue vidHeightStr) !
      src (toValue url) !
      customAttribute "frameborder" "0" !
      customAttribute "allowfullscreen" "" $
      H.p ""

youtube :: MacroFunc
youtube args attr target (Format f) _
  | f `elem` ["html", "html5", "revealjs"] = embedYoutubeHtml args attr target
youtube _ attr (vid, _) _ _ =
  Link nullAttr [Image attr [Str text] (imageUrl, "")] (videoUrl, "")
  where
    videoUrl =
      printf
        "https://www.youtube.com/embed/%s?iv_load_policy=3&disablekb=0&rel=0&modestbranding=1&autohide=1"
        vid :: String
    imageUrl =
      printf "http://img.youtube.com/vi/%s/maxresdefault.jpg" vid :: String
    text = printf "YouTube: %s" vid :: String

fontAwesome :: MacroFunc
fontAwesome _ _ (iconName, _) (Format f) _
  | f `elem` ["html", "html5", "revealjs"] =
    RawInline (Format "html") $ "<i class=\"fa fa-" ++ iconName ++ "\"></i>"
fontAwesome _ _ (iconName, _) _ _ = Str $ "[" ++ iconName ++ "]"

metaValue :: MacroFunc
metaValue _ _ (key, _) _ meta =
  case splitOn "." key of
    [] -> Str key
    k:ks -> lookup' ks (lookupMeta k meta)
  where
    lookup' :: [String] -> Maybe MetaValue -> Inline
    lookup' [] (Just (MetaString s)) = Str s
    lookup' [] (Just (MetaInlines i)) = Span nullAttr i
    lookup' (k:ks) (Just (MetaMap metaMap)) = lookup' ks (Map.lookup k metaMap)
    lookup' _ _ = Strikeout [Str key]

type MacroMap = Map.Map String MacroFunc

macroMap :: MacroMap
macroMap =
  Map.fromList [("meta", metaValue), ("youtube", youtube), ("fa", fontAwesome)]

readDefault :: Read a => a -> String -> a
readDefault default_ string = fromMaybe default_ (readMaybe string)

macroArg :: Int -> [String] -> String -> String
macroArg n args default_ =
  if length args > n
    then args !! n
    else default_

parseMacro :: String -> Maybe [String]
parseMacro (pre:invocation)
  | pre == ':' = Just (words invocation)
parseMacro _ = Nothing

isMacro :: String -> Bool
isMacro (pre:_) = pre == ':'
isMacro _ = False

onlyStrings :: [Inline] -> [String]
onlyStrings = reverse . foldl only []
  where
    only ss (Str s) = s : ss
    only ss _ = ss

expand :: Inline -> Format -> Meta -> Maybe Inline
expand (Link attr text target) format meta =
  expand_ attr text target format meta
expand x _ _ = Just x

expand_ :: Attr -> [Inline] -> Target -> Format -> Meta -> Maybe Inline
expand_ attr text target format meta = do
  name:args <- parseMacro $ stringify text
  func <- Map.lookup name macroMap
  return (func args attr target format meta)

expandInlineMacros :: Format -> Meta -> Inline -> Inline
expandInlineMacros format meta inline =
  fromMaybe inline (expand inline format meta)

expandMacros :: Format -> Pandoc -> Pandoc
expandMacros format doc@(Pandoc meta _) =
  walk (expandInlineMacros format meta) doc

isSlideHeader :: Block -> Bool
isSlideHeader (Header level _ _) = level == 1
isSlideHeader HorizontalRule = True
isSlideHeader _ = False

isBoxDelim :: Block -> Bool
isBoxDelim (Header level _ _) = level >= 2
isBoxDelim _ = False

-- Column break is either "###" or "---"
isColumnBreak :: Block -> Bool
isColumnBreak (Header level _ _) = level == 3
isColumnBreak _ = False

columnClass :: Attr
columnClass = ("", ["column"], [])

-- Splits the body of a slide into any number of columns.
splitColumns :: [Block] -> [Block]
splitColumns slide@(header:body) =
  let columns = splitWhen isColumnBreak body
      count = length columns
  in if count > 1
       then header :
            concatMap
              (\(column, n) ->
                 [ Div
                     ( ""
                     , [ "slide-column"
                       , printf "column-%d" n
                       , printf "columns-%d" count
                       ]
                     , [])
                     column
                 ])
              (Prelude.zip columns [(1 :: Int) ..])
       else slide
splitColumns [] = []

-- All fragment related classes from reveal.js have to be moved to the enclosing
-- DIV element. Otherwise to many fragments are produced.
fragmentRelated :: [String]
fragmentRelated =
  [ "fragment"
  , "grow"
  , "shrink"
  , "roll-in"
  , "fade-in"
  , "fade-out"
  , "current-visible"
  , "highlight-current-blue"
  , "highlight-red"
  , "highlight-green"
  , "highlight-blu"
  ]

deFragment :: [String] -> [String]
deFragment = filter (`notElem` fragmentRelated)

deconstructSlide :: [Block] -> (Maybe Inline, Maybe Block, [Block])
deconstructSlide (header:body) =
  case header of
    Header 1 attribs inlines ->
      ( listToMaybe $ query allImages inlines
      , Just $ Header 1 attribs (map zapImages inlines)
      , body)
deconstructSlide blocks = (Nothing, Nothing, blocks)

allImages image@Image {} = [image]
allImages _ = []

zapImages Image {} = Space
zapImages inline = inline

-- Transform inline image or video elements within the header line with
-- background attributes of the respective section. 
setSlideBackground :: [Block] -> [Block]
setSlideBackground slide@((Header 1 (headerId, headerClasses, headerAttributes) inlines):slideBody) =
  case query allImages inlines of
    [] -> slide
    Image (_, imageClasses, imageAttributes) _ (imageSrc, _):_ ->
      Header
        1
        ( headerId
        , headerClasses ++ imageClasses
        , srcAttribute imageSrc :
          headerAttributes ++ map transform imageAttributes)
        (walk zapImages inlines) :
      slideBody
  where
    transform ("size", value) = ("data-background-size", value)
    transform ("position", value) = ("data-background-position", value)
    transform ("repeat", value) = ("data-background-repeat", value)
    transform ("loop", value) = ("data-background-video-loop", value)
    transform ("muted", value) = ("data-background-video-muted", value)
    transform ("color", value) = ("data-background-color", value)
    transform ("interactive", value) = ("data-background-interactive", value)
    transform kv = kv
    srcAttribute src =
      case classifyFilePath src of
        VideoMedia -> ("data-background-video", src)
        AudioMedia -> ("data-background-audio", src)
        IframeMedia -> ("data-background-iframe", src)
        ImageMedia -> ("data-background-image", src)
setSlideBackground slide = slide

-- | Wrap boxes around H2 headers and the dollowing content. All attributes are
-- promoted from the H2 header to the enclosing DIV.
wrapBoxes :: [Block] -> [Block]
wrapBoxes (header:body) = header : concatMap wrap boxes
  where
    boxes = split (keepDelimsL $ whenElt isBoxDelim) body
    wrap (Header 2 (id_, cls, kvs) text:blocks) =
      [ Div
          (id_ ++ "-box", "box" : cls, [])
          (Header 2 (id_, deFragment cls, kvs) text : blocks)
      ]
    wrap box = box
wrapBoxes [] = []

-- | Wrap H1 headers with class notes into a DIV and promote all header
-- attributes to the DIV.
wrapNoteRevealjs :: [Block] -> [Block]
wrapNoteRevealjs slide@(Header 1 (id_, cls, kvs) inlines:body)
  | "notes" `elem` cls = [Div (id_, cls, kvs) slide]
wrapNoteRevealjs slide = slide

-- | Wrap H1 headers with class notes into a DIV and promote all header
-- attributes to the DIV.
wrapNoteBeamer :: [Block] -> [Block]
wrapNoteBeamer slide@(Header 1 (_, cls, _) _:_)
  | "notes" `elem` cls = [Div nullAttr slide]
wrapNoteBeamer slide = slide

mapSlides :: ([Block] -> [Block]) -> Pandoc -> Pandoc
mapSlides func (Pandoc meta blocks) = Pandoc meta (concatMap func slides)
  where
    slides = split (keepDelimsL $ whenElt isSlideHeader) blocks

makeSlides :: Format -> Pandoc -> Pandoc
makeSlides (Format "revealjs") =
  walk (mapSlides splitColumns) .
  walk (mapSlides setSlideBackground) .
  walk (mapSlides wrapBoxes) . walk (mapSlides wrapNoteRevealjs)
makeSlides (Format "beamer") =
  walk (mapSlides splitColumns) .
  walk (mapSlides wrapBoxes) . walk (mapSlides wrapNoteBeamer)
makeSlides _ = Prelude.id

makeBoxes :: Pandoc -> Pandoc
makeBoxes = walk (mapSlides wrapBoxes)

-- Only consider slides that have the 'notes' class in their header. In all
-- others pick only the boxes that are tagged as notes.
filterSlides :: [Block] -> [Block]
filterSlides slide@(Header 1 (_, cls, _) _:_)
  | "notes" `elem` cls = slide
filterSlides (_:body) = concatMap filter boxes
  where
    boxes = split (keepDelimsL $ whenElt isBoxDelim) body
    filter box@(Header _ (_, cls, _) _:_)
      | "notes" `elem` cls = box
    filter _ = []
filterSlides _ = []

filterNotes :: Maybe Format -> Pandoc -> Pandoc
filterNotes (Just (Format _)) = walk (mapSlides filterSlides)
filterNotes _ = Prelude.id

escapeToFilePath :: String -> FilePath
escapeToFilePath = map repl
  where
    repl c =
      if c `elem` [':', '!', '/']
        then '|'
        else c

useCachedImages :: FilePath -> Inline -> IO Inline
useCachedImages cacheDir img@(Image (ident, cls, values) inlines (url, title)) = do
  let cached = cacheDir </> escapeToFilePath url
  exists <- doesFileExist cached
  if exists
    then return (Image (ident, "cached" : cls, values) inlines (cached, title))
    else return img
useCachedImages _ inline = return inline

localImagePath :: Inline -> [FilePath]
localImagePath (Image _ _ (url, _)) =
  if isHttpUri url
    then []
    else [url]
localImagePath _ = []

extractLocalImagePathes :: Pandoc -> [FilePath]
extractLocalImagePathes = Text.Pandoc.Walk.query localImagePath

isHttpUri :: String -> Bool
isHttpUri url =
  case parseURI url of
    Just uri -> uriScheme uri `elem` ["http:", "https:"]
    Nothing -> False

cachePandocImages :: FilePath -> Inline -> IO Inline
cachePandocImages base img@(Image _ _ (url, _))
  | isHttpUri url = do
    cacheImageIO url base
    return img
  | otherwise = return img
cachePandocImages _ inline = return inline

-- | Downloads the image behind the URI and saves it locally. Returns the path of
-- the cached file relative to the base directory.
cacheImageIO :: String -> FilePath -> IO ()
cacheImageIO uri cacheDir = do
  request <- parseRequest uri
  result <- httpLBS request
  let body = getResponseBody result
  let cacheFile = cacheDir </> escapeToFilePath uri
  createDirectoryIfMissing True cacheDir
  L8.writeFile cacheFile body

renderMediaTags :: Disposition -> Pandoc -> Pandoc
renderMediaTags disposition = walk (renderImageAudioVideoTag disposition)

-- | File extensions that signify video content.
videoExtensions :: [String]
videoExtensions =
  [".mp4", ".m4v", ".webm", ".ogg", ".avi", ".dv", ".mp2", ".mov", ".qt"]

-- | File extensions that signify audio content.
audioExtensions :: [String]
audioExtensions = [".m4a", ".mp3", ".ogg", ".wav"]

-- | File extensions that signify iframe content.
iframeExtensions :: [String]
iframeExtensions = [".html", ".html", ".pdf"]

data Disposition
  = Deck
  | Page
  | Handout
  deriving (Eq)

data MediaType
  = ImageMedia
  | AudioMedia
  | VideoMedia
  | IframeMedia

uriPathExtension :: String -> String
uriPathExtension path = 
  case U.parseRelativeReference path of
    Nothing -> takeExtension path
    Just uri -> takeExtension (U.uriPath uri)

classifyFilePath :: FilePath -> MediaType
classifyFilePath name =
  case uriPathExtension name of
    ext
      | ext `elem` videoExtensions -> VideoMedia
    ext
      | ext `elem` audioExtensions -> AudioMedia
    ext
      | ext `elem` iframeExtensions -> IframeMedia
    _ -> ImageMedia

-- Renders an image with a video reference to a video tag in raw HTML. Faithfully
-- transfers attributes to the video tag.
renderImageAudioVideoTag :: Disposition -> Inline -> Inline
renderImageAudioVideoTag disposition (Image (ident, cls, values) inlines (url, tit)) =
  RawInline (Format "html") (renderHtml imageVideoTag)
  where
    imageVideoTag =
      case classifyFilePath url of
        VideoMedia -> mediaTag (video "Browser does not support video.")
        AudioMedia -> mediaTag (audio "Browser does not support audio.")
        IframeMedia -> mediaTag (iframe "Browser does not support iframe.")
        ImageMedia -> mediaTag img
    appendAttr element (key, value) =
      element ! customAttribute (stringTag key) (toValue value)
    mediaTag tag =
      ifNotEmpty A.id ident $
      ifNotEmpty class_ (unwords cls) $
      ifNotEmpty alt (stringify inlines) $
      ifNotEmpty title tit $ foldl appendAttr tag transformedValues
    ifNotEmpty attr value element =
      if value == ""
        then element
        else element ! attr (toValue value)
    srcAttr =
      if disposition == Deck
        then "data-src"
        else "src"
    transformedValues = (lazyLoad . transformImageSize) values
    lazyLoad vs = (srcAttr, url) : vs
renderImageAudioVideoTag _ inline = inline

-- | Mimic pandoc for handling the 'width' and 'height' attributes of images.
-- That is, transfer 'width' and 'height' attribute values to css style values
-- and add them to the 'style' attribute value.
transformImageSize :: [(String, String)] -> [(String, String)]
transformImageSize attributes =
  let style :: [String]
      style =
        delete "" $
        split (dropDelims $ oneOf ";") $
        fromMaybe "" $ snd <$> find (\(k, _) -> k == "style") attributes
      unstyled :: [(String, String)]
      unstyled = filter (\(k, v) -> k /= "style") attributes
      unsized =
        filter (\(k, v) -> k /= "width") $
        filter (\(k, v) -> k /= "height") unstyled
      size =
        ( snd <$> find (\(k, _) -> k == "width") unstyled
        , snd <$> find (\(k, _) -> k == "height") unstyled)
      sizeStyle =
        case size of
          (Just w, Just h) -> ["width:" ++ w, "height:" ++ h]
          (Just w, Nothing) -> ["width:" ++ w, "height:auto"]
          (Nothing, Just h) -> ["width:auto", "height:" ++ h]
          (Nothing, Nothing) -> []
      css = style ++ sizeStyle
      styleAttr = ("style", intercalate ";" $ reverse $ "" : css)
  in if null css
       then unstyled
       else styleAttr : unsized

-- | Moves the `src` attribute to `data-src` to enable reveal.js lazy loading.
lazyLoadImage :: Inline -> IO Inline
lazyLoadImage (Image (ident, cls, values) inlines (url, tit)) = do
  let kvs = ("data-src", url) : [kv | kv <- values, "data-src" /= fst kv]
  return (Image (ident, cls, kvs) inlines ("", tit))
lazyLoadImage inline = return inline
