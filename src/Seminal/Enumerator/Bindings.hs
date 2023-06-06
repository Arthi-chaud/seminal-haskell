module Seminal.Enumerator.Bindings (enumerateChangesInBinding, enumerateChangesInFuncBinding) where
import Seminal.Enumerator.Enumerator(Enumerator) 
import GHC (
    GhcPs,
    HsBindLR (..),
    SrcSpanAnn' (SrcSpanAnn, locA),
    MatchGroup (MG),
    HsBind,
    GenLocated (L), Match (..), LHsExpr, GRHSs (GRHSs), GRHS (GRHS),
    HsLocalBinds, HsLocalBindsLR (HsValBinds, HsIPBinds),
    HsValBindsLR (XValBindsLR, ValBinds), noSrcSpan, HsIPBinds (IPBinds), IPBind (IPBind)
    )
import Seminal.Change (wrapLoc, newChange, ChangeType (Removal))
import Seminal.Enumerator.Patterns (enumerateChangesInPattern)
import Data.Functor ((<&>))
import Data.List.HT (splitEverywhere)
import GHC.Data.Bag (bagToList, listToBag)
import Seminal.Enumerator.Signatures (enumerateChangeInSignature)
import Seminal.Enumerator.Expressions (enumerateChangesInExpression)

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

-- | Enumeration of changes for bindings, i.e. anything with an `=`
-- See [API doc](https://hackage.haskell.org/package/ghc-9.6.1/docs/Language-Haskell-Syntax-Binds.html#t:HsBindLR)
enumerateChangesInBinding :: Enumerator (HsBind GhcPs)
enumerateChangesInBinding (FunBind a b c d) l = enumerateChangesInFuncBinding (FunBind a b c d) l
enumerateChangesInBinding (PatBind a (L loc pat) c d) _ = enumerateChangesInPattern pat (locA loc)
    <&> fmap (L loc)
    <&> fmap (\b -> PatBind a b c d)
enumerateChangesInBinding (VarBind _ _ _) loc = []
enumerateChangesInBinding (AbsBinds {}) loc = []
enumerateChangesInBinding (PatSynBind {}) loc = []

-- | Enumerates changes to apply on function binding, e.g. `a True = True`.
-- One function binding groups all the matches
-- Basically get changes for each match
enumerateChangesInFuncBinding :: Enumerator (HsBind GhcPs)
enumerateChangesInFuncBinding (FunBind a b (MG c1 (L la ats) c3) d) _ = concat $ splitEverywhere ats
    <&> (\(h, L l e, t) -> let (SrcSpanAnn ep loc) = l in enumerateChangesInMatch e loc
            <&> wrapLoc (L . SrcSpanAnn ep)
            <&> fmap (\r ->  h ++ [r] ++ t)
            <&> fmap (\c2 -> FunBind a b (MG c1 (L la c2) c3) d)
    )
enumerateChangesInFuncBinding _ _ = []


-- | Enumeration of changes for local bindings, e.g. in a `let` or `where` clause
-- See [API doc](https://hackage.haskell.org/package/ghc-9.6.1/docs/Language-Haskell-Syntax-Binds.html#t:HsLocalBindsLR)
enumerateChangesInLocalBinds :: Enumerator (HsLocalBinds GhcPs)
enumerateChangesInLocalBinds (HsValBinds ext valbind) _ = case valbind of
    XValBindsLR {} -> []
    (ValBinds xbind binds signatures) -> enumSignatures ++ enumBinds
        where
            enumBinds = concat $ splitEverywhere (bagToList binds)
                <&> (\(h, L lbind bind, t) -> enumerateChangesInBinding bind (locA lbind)
                        <&> fmap (L lbind)
                        <&> fmap (\r ->  listToBag $ h ++ [r] ++ t)
                        <&> fmap (\b -> ValBinds xbind b signatures)
                        <&> fmap (HsValBinds ext)
                )
            enumSignatures = concat $ splitEverywhere signatures
                <&> (\(h, L lsig sign, t) -> enumerateChangeInSignature sign (locA lsig)
                        <&> fmap (L lsig)
                        <&> fmap (\r ->  h ++ [r] ++ t)
                        <&> fmap (HsValBinds ext . ValBinds xbind binds)
                )
enumerateChangesInLocalBinds (HsIPBinds ext implicitbind) l = case implicitbind of
    IPBinds xbind bindlist -> splitEverywhere bindlist
        <&> (\(h, L lbind bind, t) -> newChange
            bindlist
            (h ++ t) -- Remove bind
            l
            (case bind of
                IPBind bext lname (L lexpr expr) ->
                    enumerateChangesInExpression expr (locA lexpr)
                    <&> fmap (L lexpr)
                    <&> fmap (IPBind bext lname)
                    <&> fmap (L lbind)
                    <&> fmap (\b -> h ++ [b] ++ t)
                _ -> []
            )
            Nothing
            Removal
        )
        <&> fmap (HsIPBinds ext . IPBinds xbind)
    _ -> []
-- The other cases (`EmptyLocalBinds`, and extensions) do not need to be considered 
enumerateChangesInLocalBinds _ _ = []