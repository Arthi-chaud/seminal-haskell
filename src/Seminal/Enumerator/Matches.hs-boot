module Seminal.Enumerator.Matches (
    enumerateChangesInMatch
) where

import Seminal.Enumerator.Enumerator (Enumerator)
import GHC (GhcPs, Match, LHsExpr)

enumerateChangesInMatch :: Enumerator (Match GhcPs (LHsExpr GhcPs))