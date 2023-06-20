{-# LANGUAGE CPP #-}
module Seminal.Enumerator.Patterns (enumerateChangesInPattern) where
import Seminal.Enumerator.Enumerator (Enumerator)
import GHC (Pat(..), GhcPs, GenLocated (L), SrcSpanAnn' (locA), noExtField)
import Seminal.Enumerator.Literals (enumerateChangeInLiteral)
import Seminal.Change (Change(..), ChangeType (Wildcard), (<&&>), node)

-- | Enumerate possible changes for patterns,
-- See [API doc](https://hackage.haskell.org/package/ghc-9.6.1/docs/Language-Haskell-Syntax-Pat.html#t:Pat)
enumerateChangesInPattern :: Enumerator (Pat GhcPs)
enumerateChangesInPattern (WildPat _) _ = []
enumerateChangesInPattern pat loc = wildpatChange : case pat of
    (LitPat xlit litExpr) -> enumerateChangeInLiteral litExpr loc <&&> (LitPat xlit)
#if MIN_VERSION_ghc_lib(9,2,8)
    (ParPat xpar p1 (L lpat subpat) p2) ->   
#else
    (ParPat xpar (L lpat subpat)) ->
#endif
        enumerateChangesInPattern subpat (locA lpat) <&&> (L lpat)
#if MIN_VERSION_ghc_lib(9,2,8)
        <&&> (\c -> ParPat xpar p1 c p2) 
#else
        <&&> (ParPat xpar)
#endif
    _ -> []
    where
        wildpatChange = Change (node pat) [node $ WildPat noExtField] loc [] "The pattern is invalid." Wildcard