module Seminal.Enumerator.Bindings (
    enumerateChangesInBinding,
    enumerateChangesInLocalBinds,
    enumerateChangesInFuncBinding
) where

import Seminal.Enumerator.Enumerator (Enumerator)
import Seminal.Compiler.API

enumerateChangesInBinding :: Enumerator (HsBind GhcPs)
enumerateChangesInFuncBinding :: Enumerator (HsBind GhcPs)
enumerateChangesInLocalBinds :: Enumerator (HsLocalBinds GhcPs)