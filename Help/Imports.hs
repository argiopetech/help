{-# LANGUAGE NoImplicitPrelude, UnicodeSyntax #-}
-- |Help.Imports provides a central location for universally shared exports, primarily
--  the non-standard Prelude.
module Help.Imports ( -- *Overrides
                      FilePath
                      -- *Re-Exports
                    , module ClassyPrelude
                    , module Data.Eq.Unicode
                    ) where

import ClassyPrelude hiding (FilePath)
import Data.Eq.Unicode

-- |Overrides the default @ClassyPrelude@ @FilePath@ for fun and profit.
type FilePath = String
