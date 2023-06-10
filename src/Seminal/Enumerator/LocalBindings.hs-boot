module Seminal.Enumerator.LocalBindings (
    enumerateChangesInLocalBinds,
) where

import Seminal.Enumerator.Enumerator (Enumerator)
import GHC (HsLocalBinds, GhcPs)

enumerateChangesInLocalBinds :: Enumerator (HsLocalBinds GhcPs)