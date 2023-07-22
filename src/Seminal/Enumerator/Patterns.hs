{-# LANGUAGE CPP #-}
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
#if MIN_VERSION_ghc(9,4,1)
    (ParPat xpar openParTok (L lpat subpat) closeParTok) ->
#else
    (ParPat xpar (L lpat subpat)) ->
#endif
        enumerateChangesInPattern subpat (locA lpat)
            <&&> (L lpat)
#if MIN_VERSION_ghc(9,4,1)
            <&&> (\newPat -> ParPat xpar openParTok newPat closeParTok)
#else
            <&&> (ParPat xpar)
#endif
    _ -> []
    where
        wildpatChange = Change (node pat) [node $ WildPat NoExtField] loc [] "The pattern is invalid." Wildcard