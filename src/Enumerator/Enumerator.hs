{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
-- | From a node in the AST, provide possible changes to apply
module Enumerator.Enumerator (enumerateChangesInDeclaration, enumerateChangesAtRoot) where
import GHC (HsDecl(..), HsBindLR (..), HsBind, GhcPs, HsExpr (ExplicitList, ExplicitTuple, HsApp, HsVar), LHsDecl, EpAnn (EpAnnNotUsed), HsTupArg (Present), MatchGroup (MG), Match (Match), Pat (WildPat), GRHSs (grhssGRHSs, grhssLocalBinds, GRHSs), GRHS(GRHS), HsLocalBinds, HsLocalBindsLR (HsIPBinds, HsValBinds, EmptyLocalBinds), LHsExpr, noExtField, reLocA, getLocAnn, noAnnSrcSpan, NoExtField (NoExtField), GhcPass (GhcPs), IdP, emptyComments, noSrcSpanA)
import Enumerator.Changes (Change (..), wrapChange, wrapLoc)
import Data.Functor ((<&>))
import Data.List.HT (splitEverywhere)
import GHC.Plugins
import GHC.Hs (SrcSpanAnn'(..))

-- | Inspired from Seminal (2006, p. 5)
type Enumerator a = a -> SrcSpan -> [Change a]

-- | Expression for `undefined`
undefinedExpression :: HsExpr GhcPs
undefinedExpression = HsVar noExtField $ L (noAnnSrcSpan noSrcSpan) (mkRdrUnqual (mkVarOcc "undefined"))

-- TODO
-- Test if removing in list fixes
-- and then apply change to candidate
-- enumerateChangesInList :: (la -> (SrcSpan, a)) -> Enumerator a -> [la] -> [Change [a]]

enumerateChangesAtRoot :: [LHsDecl GhcPs] -> [Change [LHsDecl GhcPs]]
enumerateChangesAtRoot list = splitEverywhere list <&> (\(h, L l removed, t) -> let
    (SrcSpanAnn ep removedLoc) = l
    in Change {
        location = removedLoc,
        exec = h ++ [L l wildcardDecl] ++ t, -- Removing cdeclaration in list
        followups = enumerateChangesInDeclaration removed removedLoc
            <&> wrapLoc (L . SrcSpanAnn ep)
            <&> wrapChange (\r -> h ++ [r] ++ t)
    })
    where
        wildcardDecl :: HsDecl GhcPs
        wildcardDecl = ValD NoExtField wildcardBind
        wildcardBind :: HsBind GhcPs
        wildcardBind = PatBind EpAnnNotUsed (L noSrcSpanA wildcardPattern) wildcardGRHS ([], [])
        wildcardPattern :: Pat GhcPs
        wildcardPattern= WildPat NoExtField
        wildcardGRHS :: GRHSs GhcPs (LHsExpr GhcPs)
        wildcardGRHS = GRHSs emptyComments [L noSrcSpan wildcardGRHS'] (EmptyLocalBinds NoExtField)
        wildcardGRHS' = GRHS EpAnnNotUsed [] (L noSrcSpanA undefinedExpression)


enumerateChangesInDeclaration :: Enumerator (HsDecl GhcPs)
enumerateChangesInDeclaration (TyClD _ e) loc = []
enumerateChangesInDeclaration (InstD _ _) loc = []
enumerateChangesInDeclaration (DerivD _ _) loc = []
enumerateChangesInDeclaration (ValD i e) loc = enumerateChangesInBinding e loc <&> wrapChange (ValD i)
enumerateChangesInDeclaration (SigD _ e) loc = []
enumerateChangesInDeclaration _ _ = []

enumerateChangesInBinding :: Enumerator (HsBind GhcPs)
enumerateChangesInBinding (FunBind a b c d) l = enumerateChangesInFuncBinding (FunBind a b c d) l
enumerateChangesInBinding (PatBind a (L loc pat) c d) _ = enumerateChangesInPattern pat (locA loc)
    <&> wrapChange (L loc)
    <&> wrapChange (\b -> PatBind a b c d)
enumerateChangesInBinding (VarBind _ _ _) loc = []
enumerateChangesInBinding (AbsBinds {}) loc = []
enumerateChangesInBinding (PatSynBind {}) loc = []

-- | Enumerates changes to apply on function binding.
enumerateChangesInFuncBinding :: Enumerator (HsBind GhcPs)
enumerateChangesInFuncBinding (FunBind a b (MG c1 (L la ats) c3) d) _ = splitEverywhere ats
    <&> (\(h, L l e, t) -> let (SrcSpanAnn ep loc) = l in Change {
        location = loc,
        exec = h ++ t, -- Removing cdeclaration in list
        followups = enumerateChangesInMatch e loc
            <&> wrapLoc (L . SrcSpanAnn ep)
            <&> wrapChange (\r ->  h ++ [r] ++ t)
    })
    <&> wrapChange (\c2 -> FunBind a b (MG c1 (L la c2) c3) d)
enumerateChangesInFuncBinding _ _ = []

enumerateChangesInMatch :: Enumerator (Match GhcPs (LHsExpr GhcPs))
enumerateChangesInMatch (Match x ctxt pats (GRHSs ext grhss localBinds)) _ = bindingChanges
    where
        patChanges = splitEverywhere pats
            <&> (\(h, L l e, t) -> let (SrcSpanAnn ep loc) = l in Change {
                location = loc,
                exec = h ++ t, -- Removing cdeclaration in list
                followups = enumerateChangesInPattern e loc
                    <&> wrapLoc (L . SrcSpanAnn ep)
                    <&> wrapChange (\r ->  h ++ [r] ++ t)
            })
            <&> wrapChange (\newPats -> Match x ctxt newPats (GRHSs ext grhss localBinds))
        grhsChanges = splitEverywhere grhss
            <&> (\(h, L l (GRHS grhsx p (L lbody body)), t) -> Change {
                location = l,
                exec = h ++ t, -- Removing cdeclaration in list
                followups = enumerateChangesInExpression body (locA lbody)
                    <&> wrapChange (\b -> h ++ [L l $ GRHS grhsx p (L lbody b)] ++ t)
            })
            <&> wrapChange (\grhs -> Match x ctxt pats (GRHSs ext grhs localBinds))
        localbindChanges = enumerateChangesInLocalBinds localBinds noSrcSpan
            <&> wrapChange (\newlb -> Match x ctxt pats (GRHSs ext grhss newlb))

        bindingChanges = patChanges ++ grhsChanges ++ localbindChanges

enumerateChangesInLocalBinds :: Enumerator (HsLocalBinds GhcPs)
-- TODO
enumerateChangesInLocalBinds (HsValBinds _ _) l = []
enumerateChangesInLocalBinds (HsIPBinds _ _) l = []
enumerateChangesInLocalBinds _ _ = []

enumerateChangesInPattern :: Enumerator (Pat GhcPs)
-- TODO Replace pattern by wildcard
-- TODO Enumerate change in binding
enumerateChangesInPattern _ _ = []

-- | Enumerate possible changes for expressions,
-- starting with replacing them with undefined
enumerateChangesInExpression :: Enumerator (HsExpr GhcPs)
enumerateChangesInExpression expr loc = [Change {
    location = loc,
    exec = undefinedExpression,
    followups = enumerateChangesInExpression' expr loc
}]

enumerateChangesInExpression' :: Enumerator (HsExpr GhcPs)
enumerateChangesInExpression' (ExplicitList _ [a]) loc = [
    Change { location = loc, exec = unLoc a, followups = [] } -- Singleton to Item
    ]
enumerateChangesInExpression' (ExplicitList _ elems) loc = [
    Change {
        location = loc,
        exec = ExplicitTuple EpAnnNotUsed (Present EpAnnNotUsed <$> elems) Boxed, -- List to Tuple
        followups = []
    }
    ]
enumerateChangesInExpression' (HsApp a func param) _ = enumF ++ enumParam
    where
        enumF = let (L lf f) = func in enumerateChangesInExpression f (locA lf)
            <&> wrapChange (\c -> HsApp a (L lf c) param)
        enumParam = let (L lp p) = param in enumerateChangesInExpression p (locA lp)
            <&> wrapChange (\c -> HsApp a func (L lp c))
enumerateChangesInExpression' _ _ = []