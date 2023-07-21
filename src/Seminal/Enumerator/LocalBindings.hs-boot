module Seminal.Enumerator.LocalBindings (
    enumerateChangesInLocalBinds,
) where

import Seminal.Enumerator.Enumerator (Enumerator)
import Seminal.Compiler.API

enumerateChangesInLocalBinds :: Enumerator (HsLocalBinds GhcPs)