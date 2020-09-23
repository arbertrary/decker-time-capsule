{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections #-}

module Text.Decker.Filter.Streaming where

import Control.Monad.Catch
import qualified Data.Text as Text

import Relude

import Text.Blaze.Html
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Decker.Filter.Attrib
import Text.Decker.Filter.Local
import Text.Decker.Filter.Macro
import Text.Decker.Internal.Exception
import Text.Decker.Internal.URI
import Text.Pandoc
import Text.Printf
import Text.URI (URI)
import qualified Text.URI as URI

justToList :: [Maybe a] -> [a]
justToList = reverse . justToList'
  where
    justToList' ((Just x):xs) = x : justToList xs
    justToList' _ = []

youtubeDefaults =
  [ ("cc_load_policy", "0")
  , ("controls", "2")
  , ("iv_load_policy", "3")
  , ("modestbranding", "")
  , ("rel", "0")
  , ("showinfo", "0")
  ]

-- https://developers.google.com/youtube/player_parameters?hl=de#IFrame_Player_API
youtubeParams =
  [ "cc_load_policy"
  , "color"
  , "autoplay"
  , "controls"
  , "disablekb"
  , "enablejsapi"
  , "end"
  , "fs"
  , "hl"
  , "iv_load_policy"
  , "loop"
  , "modestbranding"
  , "origin"
  , "playsinline"
  , "playlist"
  , "rel"
  , "showinfo"
  , "start"
  ]

youtubeFlags =
  [ "cc_load_policy"
  , "disablekb"
  , "autoplay"
  , "controls"
  , "enablejsapi"
  , "fs"
  , "loop"
  , "modestbranding"
  , "playsinline"
  , "rel"
  , "showinfo"
  ]

twitchParams =   [ "autoplay", "controls", "muted"]

twitchFlags = ["allowfullscreen", "frameborder", "scrolling"]

-- https://vimeo.zendesk.com/hc/en-us/articles/360001494447-Using-Player-Parameters
vimeoDefaults =
  [ ("byline", "0")
  , ("controls", "1")
  , ("dnt", "1")
  , ("fun", "0")
  , ("title", "0")
  , ("transparent", "false")
  ]

vimeoParams =
  [ "autopause"
  , "autoplay"
  , "background"
  , "byline"
  , "color"
  , "controls"
  , "dnt"
  , "fun"
  , "loop"
  , "muted"
  , "playsinline"
  , "portrait"
  , "quality"
  , "speed"
  , "textrack"
  , "title"
  , "transparent"
  ]

vimeoFlags =
  [ "autopause"
  , "autoplay"
  , "background"
  , "byline"
  , "controls"
  , "dnt"
  , "fun"
  , "loop"
  , "muted"
  , "playsinline"
  , "portrait"
  , "speed"
  , "title"
  , "transparent"
  ]

-- TODO this is just an adapter for the old stuff
streamHtml :: URI -> [Inline] -> Attrib Html
streamHtml uri caption = do
  let scheme = uriScheme uri
  width <- srcAttribute "width"
  height <- srcAttribute "height"
  let args = maybe [] (\w -> maybe [w] (\h -> [w, h]) height) width
  attr <- src
  streamId <-
    case URI.uriAuthority uri of
      Right (URI.Authority _ host _) -> pure $ URI.unRText host
      _ -> return $ uriPath uri
  return $
    toHtml $ embedWebVideosHtml (fromMaybe "" scheme) args attr (streamId, "")

mediaRegex = "[a-z0-9]+" :: String
videoRegex = "[0-9]+" :: String

streamHtml' :: URI -> [Inline] -> Attrib Html
streamHtml' uri caption = do
  let scheme = uriScheme uri
  let streamId = uriPath uri
  streamUri <-
    case scheme of
      Just "youtube" -> mkYoutubeUri streamId
      Just "vimeo" -> mkVimeoUri streamId
      Just "twitch" -> mkTwitchUri streamId
      Just "veer" -> mkVeerUri streamId
      Just "veer-photo" -> mkVeerPhotoUri streamId
      _ ->
        throwM $
        ResourceException $
        "Unsupported stream service: " <> toString (fromMaybe "<none>" scheme)
  iframeAttr <- takeIframeAttr >> extractAttr
  wrapperAttr <- takeWrapperAttr >> extractAttr
  let streamTag = mkStreamTag streamUri wrapperAttr iframeAttr
  let yt = 
        case scheme of 
            Just "youtube" -> True
            _ -> False
  let placeholder = 
        case yt of 
          True -> H.img ! A.class_ "video-placeholder" ! A.src source ! A.width "520px" >> embedCaption
          False -> embedCaption
        where 
          source = H.preEscapedToValue (printf "http://img.youtube.com/vi/%s/maxresdefault.jpg" (Text.unpack streamId) :: String)
          embedCaption = H.p ! A.class_ "media-caption" $ toHtml $ link ++ Text.unpack streamId
          link = 
            case scheme of 
              Just "youtube" -> "https://www.youtube.com/watch?v=" 
              Just "vimeo" -> "https://vimeo.com/" 
              Just "twitch" -> "https://player.twitch.tv/?parent=localhost&video="
              Just "veer" -> "https://veer.tv/videos/"
              Just "veer-photo" -> "https://veer.tv/photos/"
              _ -> ""
  case caption of
    [] -> do
      divAttr <- injectClass "nofigure" >> injectBorder >> takeSize >> takeUsual >> extractAttr
      return $ mkDivTag streamTag divAttr yt >> placeholder
    caption -> do
      figAttr <- injectBorder >> takeSize >> takeUsual >> extractAttr
      captionHtml <- lift $ inlinesToHtml caption
      return $ mkStreamFigureTag yt streamTag captionHtml figAttr >> placeholder

takeWrapperAttr :: Attrib ()
takeWrapperAttr = do
  aspect <- calcAspect . fromMaybe "" <$> cutAttrib "aspect"
  injectStyle ("position", "relative")
  injectStyle ("padding-top", "25px")
  injectStyle ("padding-bottom", aspect)
  injectStyle ("height", "0")

takeIframeAttr :: Attrib ()
takeIframeAttr = do
  injectStyle ("position", "absolute")
  injectStyle ("top", "0")
  injectStyle ("left", "0")
  injectStyle ("width", "100%")
  injectStyle ("height", "100%")
  injectAttribute ("allow", "fullscreen")
  takeAutoplay

mkYoutubeUri :: Text -> Attrib URI
mkYoutubeUri streamId = do
  params <- cutAttribs youtubeParams
  flags <- cutClasses youtubeFlags
  uri <- URI.mkURI $ "https://www.youtube.com/embed/" <> streamId
  setQuery [] (merge [params, map (, "1") flags, youtubeDefaults]) uri

mkVimeoUri :: Text -> Attrib URI
mkVimeoUri streamId = do
  params <- cutAttribs vimeoParams
  flags <- cutClasses vimeoFlags
  uri <- URI.mkURI $ "https://player.vimeo.com/video/" <> streamId
  setQuery [] (merge [params, map (, "1") flags, vimeoDefaults]) uri

mkTwitchUri :: Text -> Attrib URI
mkTwitchUri streamId = do
  params <- cutAttribs twitchParams
  flags <- cutClasses twitchFlags
  uri <- URI.mkURI "https://player.twitch.tv/"
  setQuery [] (merge [("video", streamId) : ("parent", "localhost") : params, map (, "1") flags]) uri

mkVeerUri :: Text -> Attrib URI
mkVeerUri streamId = do
  uri <- URI.mkURI "https://h5.veer.tv/player/"
  setQuery [] [("vid", streamId)] uri

mkVeerPhotoUri :: Text -> Attrib URI
mkVeerPhotoUri streamId = do
  uri <- URI.mkURI "https://h5.veer.tv/photo-player/"
  setQuery [] [("pid", streamId)] uri

calcAspect :: Text -> Text
calcAspect ratio =
  fromMaybe "56.25%" $
  case Text.splitOn ":" ratio of
    [w, h] -> do
      wf <- readMaybe $ toString w :: Maybe Float
      hf <- readMaybe $ toString h :: Maybe Float
      return $ Text.pack (printf "%.2f%%" (hf / wf * 100.0))
    _ -> Nothing

mkAttrTag :: Html -> Attr -> Html
mkAttrTag tag (id, cs, kvs) =
  tag !? (not (Text.null id), A.id (H.toValue id)) !?
  (not (null cs), A.class_ (H.toValue ("decker" : cs))) !*
  kvs

mkMediaTag :: Html -> URI -> Bool -> Attr -> Html
mkMediaTag tag uri dataSrc attr =
  let srcAttr =
        if dataSrc
          then H.dataAttribute "src"
          else A.src
   in mkAttrTag tag attr ! srcAttr (H.preEscapedToValue $ URI.render uri)

mkStreamTag :: URI -> Attr -> Attr -> Html
mkStreamTag uri wrapperAttr iframeAttr =
  let inner =
        mkMediaTag (H.iframe "Iframe showing video here.") uri False iframeAttr
   in mkAttrTag (H.div inner) wrapperAttr 

mkDivTag :: Html -> Attr -> Bool -> Html
mkDivTag content (id, cs, kvs) youtube =
  H.div !? (not (Text.null id), A.id (H.toValue id)) !
  A.class_ (H.toValue ("decker" : altClass : cs)) !*
  kvs $
  content
  where altClass = case youtube of 
                True -> "youtube-iframe"
                False -> ""

mkStreamFigureTag :: Bool -> Html -> Html -> Attr ->  Html
mkStreamFigureTag youtube content caption (id, cs, kvs) =
  H.figure !? (not (Text.null id), A.id (H.toValue id)) !
  A.class_ (H.toValue ("decker" : altClass : cs)) !*
  kvs $ do
    content
    H.figcaption ! A.class_ "decker" $ caption
  where altClass = case youtube of 
          True -> "youtube-iframe"
          False -> ""