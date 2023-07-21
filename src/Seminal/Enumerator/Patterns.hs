module Seminal.Enumerator.Patterns (enumerateChangesInPattern) where
import Seminal.Enumerator.Enumerator (Enumerator)
import Seminal.Compiler.API
import Seminal.Enumerator.Literals (enumerateChangeInLiteral)
import Seminal.Change (Change(..), ChangeType (Wildcard), (<&&>), node)

-- | Enumerate possible changes for patterns,
-- See [API doc](https://hackage.haskell.org/package/ghc-9.6.1/docs/Language-Haskell-Syntax-Pat.html#t:Pat)
enumerateChangesInPattern :: Enumerator (Pat GhcPs)
enumerateChangesInPattern (WildPat _) _ = []
enumerateChangesInPattern pat loc = wildpatChange : case pat of
    (LitPat xlit litExpr) -> enumerateChangeInLiteral litExpr loc <&&> (LitPat xlit)
    (ParPat xpar openParToken (L lpat subpat) closeParToken) -> enumerateChangesInPattern subpat (locA lpat)
        <&&> (L lpat)
        <&&> (\x -> ParPat xpar openParToken x closeParToken)
    _ -> []
    where
        wildpatChange = Change (node pat) [node $ WildPat noExtField] loc [] "The pattern is invalid." Wildcard