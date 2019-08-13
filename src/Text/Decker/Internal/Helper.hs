module Text.Decker.Internal.Helper
  ( dropSuffix
  , replaceSuffix
  , isPrefix
  , makeRelativeTo
  , invertPath
  , mapTuple
  , repeatIfTrue
  , whenTrue
  , unique
  , time
  , (<++>)
  , runIOQuietly
  ) where

import Control.Monad.State
import qualified Data.List.Extra as List
import Data.Maybe
import qualified Data.Set as Set
import System.CPUTime
import System.FilePath
import Text.Pandoc
import Text.Printf

runIOQuietly :: PandocIO a -> IO (Either PandocError a)
runIOQuietly act = runIO (setVerbosity ERROR >> act)

-- | Monadic version of list concatenation.
(<++>) :: Monad m => m [a] -> m [a] -> m [a]
(<++>) = liftM2 (++)

repeatIfTrue :: Monad m => m Bool -> m ()
repeatIfTrue action = do
  again <- action
  when again $ repeatIfTrue action

whenTrue :: Monad m => m Bool -> m () -> m ()
whenTrue bool action = do
  true <- bool
  when true action

-- | Removes the last suffix from a filename
dropSuffix :: String -> String -> String
dropSuffix s t = fromMaybe t (List.stripSuffix s t)

replaceSuffix :: String -> String -> String -> String
replaceSuffix srcSuffix targetSuffix filename =
  dropSuffix srcSuffix filename ++ targetSuffix

unique :: Ord a => [a] -> [a]
unique = Set.toList . Set.fromList

time :: String -> IO a -> IO a
time name action = do
  start <- getCPUTime
  result <- action
  stop <- getCPUTime
  let diff = fromIntegral (stop - start) / (10 ^ 12)
  printf "%s: %0.5f sec\n" name (diff :: Double)
  return result

isPrefix :: FilePath -> FilePath -> Bool
isPrefix prefix whole = isPrefix_ (splitPath prefix) (splitPath whole)
  where
    isPrefix_ :: Eq a => [a] -> [a] -> Bool
    isPrefix_ (a:as) (b:bs)
      | a == b = isPrefix_ as bs
      | otherwise = False
    isPrefix_ [] _ = True
    isPrefix_ _ _ = False

-- | Express the second path argument as relative to the first. 
-- Both arguments are expected to be absolute pathes. 
makeRelativeTo :: FilePath -> FilePath -> FilePath
makeRelativeTo dir file =
  let (d, f) = removeCommonPrefix (normalise dir, normalise file)
   in normalise $ invertPath d </> f

invertPath :: FilePath -> FilePath
invertPath fp = joinPath $ map (const "..") $ filter ("." /=) $ splitPath fp

removeCommonPrefix :: (FilePath, FilePath) -> (FilePath, FilePath)
removeCommonPrefix =
  mapTuple joinPath . removeCommonPrefix_ . mapTuple splitDirectories
  where
    removeCommonPrefix_ :: ([FilePath], [FilePath]) -> ([FilePath], [FilePath])
    removeCommonPrefix_ (al@(a:as), bl@(b:bs))
      | a == b = removeCommonPrefix_ (as, bs)
      | otherwise = (al, bl)
    removeCommonPrefix_ pathes = pathes

mapTuple :: (t1 -> t) -> (t1, t1) -> (t, t)
mapTuple f (a, b) = (f a, f b)
