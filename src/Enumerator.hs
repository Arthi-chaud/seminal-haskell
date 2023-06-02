{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
-- | From a node in the AST, provide possible changes to apply
module Enumerator (enumerateChangesInDeclaration, enumerateChangesAtRoot) where

import GHC (HsDecl(..), HsBindLR (..), HsBind, HsExpr (ExplicitList, ExplicitTuple, HsApp, HsVar, HsLit, HsOverLit), LHsDecl, EpAnn (EpAnnNotUsed), HsTupArg (Present), MatchGroup (MG), Match (Match), Pat (WildPat), GRHSs (grhssGRHSs, grhssLocalBinds, GRHSs), GRHS(GRHS), HsLocalBinds, HsLocalBindsLR (HsIPBinds, HsValBinds), LHsExpr, noExtField, noAnnSrcSpan, GhcPs, SrcSpanAnn' (..), HsLit (HsChar, HsString, HsCharPrim, HsStringPrim), getLocA)
import Changes (Change (..), wrapChange, wrapLoc)
import Data.Functor ((<&>))
import Data.List.HT (splitEverywhere)
import GHC.Plugins
    ( noSrcSpan,
      unLoc,
      GenLocated(L),
      SrcSpan,
      mkVarOcc,
      mkRdrUnqual,
      Boxity(Boxed), mkFastString, unpackFS, trace )
import Data.ByteString.Internal (w2c)
import Data.ByteString (unpack)
import GHC.Types.SourceText (SourceText(NoSourceText))
import GHC.Hs (noSrcSpanA)

-- | Inspired from Seminal (2006, p. 5)
type Enumerator a = a -> SrcSpan -> [Change a]

-- | Expression for `undefined`
undefinedExpression :: HsExpr GhcPs
undefinedExpression = HsVar noExtField $ L (noAnnSrcSpan noSrcSpan) (mkRdrUnqual (mkVarOcc "undefined"))

-- wildcardDecl :: HsDecl GhcPs
-- wildcardDecl = ValD NoExtField wildcardBind
--     where
--         wildcardBind = PatBind EpAnnNotUsed (L noSrcSpanA wildcardPattern) wildcardGRHS ([], [])
--         wildcardPattern= WildPat NoExtField
--         wildcardGRHS = GRHSs emptyComments [L noSrcSpan wildcardGRHS'] (EmptyLocalBinds NoExtField)
--         wildcardGRHS' = GRHS EpAnnNotUsed [] (L noSrcSpanA undefinedExpression)

enumerateChangesAtRoot :: [LHsDecl GhcPs] -> [Change [LHsDecl GhcPs]]
enumerateChangesAtRoot list = concat $ splitEverywhere list <&> (\(h, L l removed, t) -> let
    (SrcSpanAnn ep removedLoc) = l
    followups = enumerateChangesInDeclaration removed removedLoc
        <&> wrapLoc (L . SrcSpanAnn ep)
        <&> wrapChange (\r -> h ++ [r] ++ t)
    buildChange exec = Change removedLoc list exec followups
    in case removed of
        (ValD v (FunBind a b c d)) -> enumerateChangesInFuncBinding (FunBind a b c d) removedLoc
            <&> wrapChange (L l . ValD v)
            <&> wrapChange (\change -> h ++ [change] ++ t)
        _ -> [buildChange $ h ++ t]
    )

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
-- TODO: Remove decl only if there is more than one
enumerateChangesInFuncBinding :: Enumerator (HsBind GhcPs)
enumerateChangesInFuncBinding (FunBind a b (MG c1 (L la ats) c3) d) _ = concat $ splitEverywhere ats
    <&> (\(h, L l e, t) -> let (SrcSpanAnn ep loc) = l in enumerateChangesInMatch e loc
            <&> wrapLoc (L . SrcSpanAnn ep)
            <&> wrapChange (\r ->  h ++ [r] ++ t)
            <&> wrapChange (\c2 -> FunBind a b (MG c1 (L la c2) c3) d)
    )
enumerateChangesInFuncBinding _ _ = []

enumerateChangesInMatch :: Enumerator (Match GhcPs (LHsExpr GhcPs))
enumerateChangesInMatch (Match x ctxt pats (GRHSs ext grhss localBinds)) _ = bindingChanges
    where
        patChanges = splitEverywhere pats
            <&> (\(h, L l e, t) -> let (SrcSpanAnn ep loc) = l in Change {
                location = loc,
                src = pats,
                exec = h ++ t, -- Removing declaration in list
                followups = enumerateChangesInPattern e loc
                    <&> wrapLoc (L . SrcSpanAnn ep)
                    <&> wrapChange (\r ->  h ++ [r] ++ t)
            })
            <&> wrapChange (\newPats -> Match x ctxt newPats (GRHSs ext grhss localBinds))
        grhsChanges = concat (splitEverywhere grhss
            <&> (\(h, L l (GRHS grhsx p (L lbody body)), t) -> enumerateChangesInExpression body (locA lbody)
                    <&> wrapChange (\b -> h ++ [L l $ GRHS grhsx p (L lbody b)] ++ t)
            ))
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
-- starting with replacing them with undefined, and wr
enumerateChangesInExpression :: Enumerator (HsExpr GhcPs)
enumerateChangesInExpression expr loc = [
    Change {
        location = loc,
        src = expr,
        exec = undefinedExpression,
        followups = enumerateChangesInExpression' expr loc ++ [
            -- Try to wrap item in list
            Change {
                location = loc,
                src = expr,
                exec = ExplicitList EpAnnNotUsed [L noSrcSpanA expr],
                followups = []
            }]
    }]

enumerateChangesInExpression' :: Enumerator (HsExpr GhcPs)
enumerateChangesInExpression' expr loc =  case expr of
    (ExplicitList _ [a]) -> [
        Change {
            location = loc,
            src = expr,
            exec = unLoc a,
            followups = []
        } -- Singleton to Item
        ]
    (ExplicitList _ elems) -> [
        Change {
            location = loc,
            src = expr,
            exec = ExplicitTuple EpAnnNotUsed (Present EpAnnNotUsed <$> elems) Boxed,
            followups = []
        } -- List to Tuple
        ]
    -- In function application: try chnages on functions and parameters
    (HsApp a func param) -> enumF ++ enumParam
        where
            enumF = let (L lf f) = func in enumerateChangesInExpression f (locA lf)
                <&> wrapChange (\c -> HsApp a (L lf c) param)
            enumParam = let (L lp p) = param in enumerateChangesInExpression p (locA lp)
                <&> wrapChange (HsApp a func . L lp)
    -- Attempts tweaks with litterals
    (HsLit ext literal) -> enumerateChangeInLiteral literal loc
        <&> wrapChange (HsLit ext)
    _ -> []

-- | Enumeration of changes for Literals
enumerateChangeInLiteral :: Enumerator (HsLit GhcPs)
enumerateChangeInLiteral literal loc = case literal of
    (HsChar _ char) -> changeForChar char
    (HsCharPrim _ char) -> changeForChar char
    (HsString _ string) -> changeForString $ unpackFS string
    (HsStringPrim _ string) -> changeForString $ w2c <$> unpack string
    _ -> []
    where
        changeForChar char = [Change {
            location = loc,
            src = literal,
            exec = HsString NoSourceText (mkFastString [char]),
            followups = []
        }]
        changeForString [char] = [Change {
            location = loc,
            src = literal,
            exec = HsChar NoSourceText char,
            followups = []
        }]
        changeForString _ = []