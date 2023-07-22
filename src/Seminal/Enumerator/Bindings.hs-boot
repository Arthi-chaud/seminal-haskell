module Seminal.Enumerator.Bindings (
    enumerateChangesInBinding,
    enumerateChangesInFuncBinding
) where

import Seminal.Enumerator.Enumerator (Enumerator)
import GHC (HsBind, GhcPs)

enumerateChangesInBinding :: Enumerator (HsBind GhcPs)
enumerateChangesInFuncBinding :: Enumerator (HsBind GhcPs)