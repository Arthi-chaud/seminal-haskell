module Seminal.Enumerator.Expressions (
    enumerateChangesInExpression
) where
import Seminal.Enumerator.Enumerator (Enumerator)
import GHC (HsExpr, GhcPs)

enumerateChangesInExpression :: Enumerator (HsExpr GhcPs)