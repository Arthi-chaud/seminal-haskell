module Seminal.Enumerator.Bindings (
    enumerateChangesInBinding,
    enumerateChangesInLocalBinds,
    enumerateChangesInFuncBinding
) where

import Seminal.Enumerator.Enumerator (Enumerator)
import GHC (HsBind, GhcPs)
import GHC.Hs (HsLocalBinds)

enumerateChangesInBinding :: Enumerator (HsBind GhcPs)
enumerateChangesInFuncBinding :: Enumerator (HsBind GhcPs)
enumerateChangesInLocalBinds :: Enumerator (HsLocalBinds GhcPs)