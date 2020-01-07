{-- Author: Jan-Philipp Stauffert <jan-philipp.stauffert@uni-wuerzburg.de.de> --}
-- {-# LANGUAGE CPP #-}

module Text.Decker.Internal.Flags
  ( hasPreextractedResources
  ) where

hasPreextractedResources :: Bool
hasPreextractedResources = False
-- #ifdef PREEXTRACTEDRESOURCES
-- hasPreextractedResources = True
-- #else
-- hasPreextractedResources = False
-- #endif
