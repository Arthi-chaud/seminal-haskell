module Enumerator.Matches (enumerateChangesInMatch) where
import Enumerator.Enumerator (Enumerator)
import GHC (GhcPs, GRHSs (..), Match (..), GenLocated (L), SrcSpanAnn' (..), GRHS (GRHS), noSrcSpan)
import GHC.Hs (LHsExpr)
import Data.List.HT (splitEverywhere)
import Change (newChange, wrapLoc, ChangeType (Wildcard, Removal))
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
                    <&> fmap (\r ->  h ++ [r] ++ t))
                Nothing Removal
            )
            <&> fmap (\newPats -> Match x ctxt newPats (GRHSs ext grhss localBinds))
        -- | Changes for the right-hand side of the `=` symbol
        -- Note: GHRS is Guarded Right-Hand Side
        grhsChanges = concat (splitEverywhere grhss
            <&> (\(h, L l (GRHS grhsx p (L lbody body)), t) ->  enumerateChangesInExpression body (locA lbody)
                    <&> fmap (L lbody)
                    <&> fmap (GRHS grhsx p)
                    <&> fmap (L l)
                    <&> fmap (\b -> h ++ [b] ++ t)
            ))
            <&> fmap (\grhs -> Match x ctxt pats (GRHSs ext grhs localBinds))
        -- | The enumeration of changes for the `where` clause of the match
        localbindChanges = enumerateChangesInLocalBinds localBinds noSrcSpan
            <&> fmap (Match x ctxt pats . GRHSs ext grhss)

        bindingChanges = patChanges ++ grhsChanges ++ localbindChanges

