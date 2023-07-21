module Seminal.Enumerator.Expressions (
    enumerateChangesInExpression
) where
import Seminal.Enumerator.Enumerator (Enumerator)
import Seminal.Compiler.API

enumerateChangesInExpression :: Enumerator (HsExpr GhcPs)