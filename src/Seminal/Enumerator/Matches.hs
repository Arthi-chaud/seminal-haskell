module Seminal.Enumerator.Matches (
    enumerateChangesInMatch
) where
import Seminal.Enumerator.Enumerator (Enumerator)
import GHC
    ( GhcPs,
      GenLocated(..),
      noSrcSpan,
      SrcSpanAnn'(locA),
      LHsExpr,
      GhcPs,
      SrcSpanAnn'(SrcSpanAnn, locA),
      GenLocated(L),
      Match(..),
      LHsExpr,
      GRHSs(GRHSs),
      GRHS(GRHS),
      noSrcSpan )
import Seminal.Change ((<&&>))
import Seminal.Enumerator.Patterns (enumerateChangesInPattern)
import Data.Functor ((<&>))
import Data.List.HT (splitEverywhere)
import Seminal.Enumerator.Expressions (enumerateChangesInExpression)
import Seminal.Enumerator.LocalBindings (enumerateChangesInLocalBinds)

-- | Enumerates changes for a single match
-- See [API doc](https://hackage.haskell.org/package/ghc-9.6.1/docs/Language-Haskell-Syntax-Expr.html#t:Match)
enumerateChangesInMatch :: Enumerator (Match GhcPs (LHsExpr GhcPs))
enumerateChangesInMatch (Match x ctxt pats (GRHSs ext grhss localBinds)) _ = bindingChanges
    where
        -- | Changes for the left-hand side of the `=` symbol
        patChanges = concat $ splitEverywhere pats
            <&> (\(h, L l e, t) -> let (SrcSpanAnn _ loc) = l in enumerateChangesInPattern e loc
                    <&&> (\r ->  h ++ [L l r] ++ t)
                    <&&> (\newPats -> Match x ctxt newPats (GRHSs ext grhss localBinds))
            )
        -- | Changes for the right-hand side of the `=` symbol
        -- Note: GHRS is Guarded Right-Hand Side
        grhsChanges = concat (splitEverywhere grhss
            <&> (\(h, L l (GRHS grhsx p (L lbody body)), t) ->  enumerateChangesInExpression body (locA lbody)
                    <&&> (L lbody)
                    <&&> (GRHS grhsx p)
                    <&&> (L l)
                    <&&> (\b -> h ++ [b] ++ t)
            ))
            <&&> (\grhs -> Match x ctxt pats (GRHSs ext grhs localBinds))
        -- | The enumeration of changes for the `where` clause of the match
        localbindChanges = enumerateChangesInLocalBinds localBinds noSrcSpan
            <&&> (Match x ctxt pats . GRHSs ext grhss)

        bindingChanges = patChanges ++ grhsChanges ++ localbindChanges
