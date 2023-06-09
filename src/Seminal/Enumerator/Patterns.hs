module Seminal.Enumerator.Patterns (enumerateChangesInPattern) where
import Seminal.Enumerator.Enumerator (Enumerator)
import GHC (Pat(..), GhcPs, GenLocated (L), SrcSpanAnn' (locA), noExtField)
import Seminal.Enumerator.Literals (enumerateChangeInLiteral)
import Seminal.Change (newChange, ChangeType (Wildcard), (<&&>))

-- | Enumerate possible changes for patterns,
-- See [API doc](https://hackage.haskell.org/package/ghc-9.6.1/docs/Language-Haskell-Syntax-Pat.html#t:Pat)
enumerateChangesInPattern :: Enumerator (Pat GhcPs)
enumerateChangesInPattern (WildPat _) _ = []
enumerateChangesInPattern pat loc = wildpatChange : case pat of
    (LitPat xlit litExpr) -> enumerateChangeInLiteral litExpr loc <&&> (LitPat xlit)
    (ParPat xpar (L lpat subpat)) -> enumerateChangesInPattern subpat (locA lpat)
        <&&> (L lpat)
        <&&> (ParPat xpar)
    _ -> []
    where
        wildpatChange = newChange pat (WildPat noExtField) loc [] Nothing Wildcard