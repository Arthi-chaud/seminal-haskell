module Seminal.Enumerator.Bindings (
    enumerateChangesInBinding,
    enumerateChangesInFuncBinding
) where

import Seminal.Enumerator.Enumerator (Enumerator)
import Seminal.Compiler.API

enumerateChangesInBinding :: Enumerator (HsBind GhcPs)
enumerateChangesInFuncBinding :: Enumerator (HsBind GhcPs)