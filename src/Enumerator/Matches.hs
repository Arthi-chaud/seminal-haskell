module Enumerator.Matches (enumerateChangesInMatch) where
import Enumerator.Enumerator (Enumerator)
import GHC (GhcPs, GRHSs (..), Match (..), GenLocated (L), SrcSpanAnn' (..), GRHS (GRHS), noSrcSpan)
import GHC.Hs (LHsExpr)
import Data.List.HT (splitEverywhere)
import Changes (newChange, wrapChange, wrapLoc)
import Data.Functor ((<&>))
import Enumerator.Patterns (enumerateChangesInPattern)
import Enumerator.Expressions (enumerateChangesInExpression)
import Enumerator.LocalBindings (enumerateChangesInLocalBinds)

-- | Enumerates changes for a single match
-- See [API doc](https://hackage.haskell.org/package/ghc-9.6.1/docs/Language-Haskell-Syntax-Expr.html#t:Match)
enumerateChangesInMatch :: Enumerator (Match GhcPs (LHsExpr GhcPs))
enumerateChangesInMatch (Match x ctxt pats (GRHSs ext grhss localBinds)) _ = bindingChanges
    where
        -- | Changes for the left-hand side of the `=` symbol
        patChanges = splitEverywhere pats
            <&> (\(h, L l e, t) -> let (SrcSpanAnn ep loc) = l in newChange
                pats
                (h ++ t) -- Removing declaration in list
                loc
                -- | Enumerate changes for the pattern
                (enumerateChangesInPattern e loc
                    <&> wrapLoc (L . SrcSpanAnn ep)
                    <&> wrapChange (\r ->  h ++ [r] ++ t))
            )
            <&> wrapChange (\newPats -> Match x ctxt newPats (GRHSs ext grhss localBinds))
        -- | Changes for the right-hand side of the `=` symbol
        -- Note: GHRS is Guarded Right-Hand Side
        grhsChanges = concat (splitEverywhere grhss
            <&> (\(h, L l (GRHS grhsx p (L lbody body)), t) ->  enumerateChangesInExpression body (locA lbody)
                    <&> wrapChange (L lbody)
                    <&> wrapChange (GRHS grhsx p)
                    <&> wrapChange (L l)
                    <&> wrapChange (\b -> h ++ [b] ++ t)
            ))
            <&> wrapChange (\grhs -> Match x ctxt pats (GRHSs ext grhs localBinds))
        -- | The enumeration of changes for the `where` clause of the match
        localbindChanges = enumerateChangesInLocalBinds localBinds noSrcSpan
            <&> wrapChange (Match x ctxt pats . GRHSs ext grhss)

        bindingChanges = patChanges ++ grhsChanges ++ localbindChanges

