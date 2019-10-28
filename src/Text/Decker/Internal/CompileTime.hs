module Text.Decker.Internal.CompileTime
  ( lookupGitBranch
  , lookupGitCommitId
  , lookupGitTag
  , lookupCompileDate
  ) where

import Data.Maybe
import Data.String.Utils
import Data.Time.Calendar
import Data.Time.Clock
import Language.Haskell.TH
import Text.Decker.Project.Git

lookupGitBranch :: Q Exp
lookupGitBranch = stringE . strip . fromMaybe "none" =<< runIO gitBranch

lookupGitCommitId :: Q Exp
lookupGitCommitId = stringE . strip . fromMaybe "none" =<< runIO gitRevision

lookupCompileDate :: Q Exp
lookupCompileDate = do
  date <- runIO $ getCurrentTime >>= return . toGregorian . utctDay
  let temp = show date
  (stringE . strip) temp

lookupGitTag :: Q Exp
lookupGitTag = stringE . strip . fromMaybe "none" =<< runIO gitRevisionTag
