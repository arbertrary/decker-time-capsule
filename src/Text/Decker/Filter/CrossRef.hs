module Text.Decker.Filter.CrossRef
  ( processCrossRefs
  ) where

import Text.Decker.Internal.Common

import Text.Pandoc
import Text.Pandoc.CrossRef
import Text.Pandoc.Walk

processCrossRefs :: Pandoc -> Decker Pandoc
processCrossRefs pandoc@(Pandoc meta blocks) = do
  let refBlocks = runCrossRef meta Nothing crossRefBlocks blocks
  return (Pandoc meta refBlocks)
