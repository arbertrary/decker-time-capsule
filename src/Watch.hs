{-- Author: Henrik Tramberend <henrik@tramberend.de> --}
module Watch
  ( waitForTwitchPassive
  ) where

-- | A non-polling file watcher based on fsnotify
import Control.Concurrent.MVar
import qualified Data.Set as Set
import Filter
import Render
import System.FSNotify
import System.FilePath

-- | Wait for something to happen on one of the matching files in one of the
-- supplied directories. TODO: Get rid of the twitchExtensions. Watch
-- everything, except the public dir.
waitForTwitch :: Bool -> [FilePath] -> IO FilePath
waitForTwitch doPolling directories = do
  print doPolling
  done <- newEmptyMVar
  mgr <- if doPolling
    then startManagerConf (WatchConfig DebounceDefault 10000 True)
    else startManager
  stops <- watchIt mgr done
  filepath <- takeMVar done
  sequence_ stops
  stopManager mgr
  return filepath
    -- Match a filepath against the supplied patterns
  where
    isWatchedFile event =
      (takeExtension . eventPath) event `elem` twitchExtensions
    -- Stop the watch manager and notify the main thread
    stopWatching _ done event = putMVar done (eventPath event)
    -- Watch everything within the supplied dirs
    watchInDir mgr done dir =
      watchTree mgr dir isWatchedFile (stopWatching mgr done)
    watchIt mgr done = mapM (watchInDir mgr done) directories

commonExtensions :: [String]
commonExtensions = [".scss", ".css", ".md", ".yaml", ".png", ".gif", ".jpg", ".svg"]

twitchExtensions :: [String]
twitchExtensions =
  commonExtensions ++
  iframeExtensions ++
  audioExtensions ++ videoExtensions ++ renderedCodeExtensions

waitForTwitchPassive :: Bool -> [FilePath] -> IO FilePath
waitForTwitchPassive doPolling files = do
  let dirs = unique (map takeDirectory files)
  waitForTwitch doPolling dirs

unique :: Ord a => [a] -> [a]
unique = Set.toList . Set.fromList
