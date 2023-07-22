module Seminal.Enumerator.Matches (
    enumerateChangesInMatch
) where

import Seminal.Enumerator.Enumerator (Enumerator)
import Seminal.Compiler.API

enumerateChangesInMatch :: Enumerator (Match GhcPs (LHsExpr GhcPs))