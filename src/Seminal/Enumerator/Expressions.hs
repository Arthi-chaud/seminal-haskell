module Seminal.Enumerator.Expressions (
    enumerateChangesInExpression,
    enumerateChangesInBinding,
    enumerateChangesInFuncBinding
) where
import Seminal.Enumerator.Enumerator (Enumerator)
import GHC
    ( HsExpr(..),
      GhcPs,
      GenLocated(..),
      noExtField,
      noSrcSpan,
      noAnnSrcSpan,
      EpAnn(EpAnnNotUsed),
      HsTupArg(Present),
      noSrcSpanA,
      SrcSpanAnn'(locA),
      LHsExpr,
      GhcPs,
      HsBindLR(..),
      SrcSpanAnn'(SrcSpanAnn, locA),
      MatchGroup(MG),
      HsBind,
      GenLocated(L),
      Match(..),
      LHsExpr,
      GRHSs(GRHSs),
      GRHS(GRHS),
      HsLocalBinds,
      HsLocalBindsLR(HsValBinds, HsIPBinds),
      HsValBindsLR(XValBindsLR, ValBinds),
      noSrcSpan,
      HsIPBinds(IPBinds),
      IPBind(IPBind) )
import Seminal.Change
    ( wrapLoc,
      newChange,
      ChangeType(Removal),
      newChange,
      rewriteSrc,
      ChangeType(Terminal, Wildcard, Wrapping, Removal),
      (<&&>)
    )
import Seminal.Enumerator.Patterns (enumerateChangesInPattern)
import Data.Functor ((<&>))
import Data.List.HT (splitEverywhere)
import GHC.Data.Bag (bagToList, listToBag)
import Seminal.Enumerator.Signatures (enumerateChangeInSignature)
import GHC.Plugins (mkRdrUnqual, mkVarOcc, Boxity (Boxed), mkDataOcc)
import Seminal.Enumerator.Literals (enumerateChangeInLiteral)
import Data.Maybe (mapMaybe)

-- | Enumerate possible changes for expressions,
-- starting with replacing them with undefined.
-- See [API doc](https://hackage.haskell.org/package/ghc-9.6.1/docs/Language-Haskell-Syntax-Expr.html#t:HsExpr)
enumerateChangesInExpression :: Enumerator (HsExpr GhcPs)
enumerateChangesInExpression expr loc = [changeToUndefined, changeToNil]
    where
        -- | Change the expression to `undefined`, used as a wildcard
        changeToUndefined = newChange expr undefinedExpression loc (changeToList:changeToString:changeToTrue:subchanges) Nothing Wildcard
        changeToNil = newChange expr (ExplicitList EpAnnNotUsed []) loc (changeToString:changeToList:subchanges) Nothing Wildcard
        -- | Wrap the expression into a list
        changeToList = newChange expr (ExplicitList EpAnnNotUsed [lexpr]) loc [] Nothing Wrapping
        -- | Try to call `show` on the Expression
        changeToString = newChange expr (HsApp EpAnnNotUsed (locMe $ buildFunctionName "show") lexpr) loc [] Nothing Wrapping
            <&> (wrapExprInPar . locMe)
        changeToTrue = newChange expr (HsVar noExtField ltrue) loc [] message Wildcard
            where
                message = return "This expression needs to evaluate to a boolean value. It is not the case here. Check the type of the value."
                ltrue = L (noAnnSrcSpan noSrcSpan) (mkRdrUnqual (mkDataOcc "True"))
        -- | The other, specalised, changes to consider
        subchanges = enumerateChangesInExpression' expr loc
        -- | Located expr
        lexpr = locMe expr
        -- | Wrap HsExpr into LHsExpr
        locMe = L noSrcSpanA

enumerateChangesInExpression' :: Enumerator (HsExpr GhcPs)
enumerateChangesInExpression' (HsPar _ (L lexpr expr)) _ = enumerateChangesInExpression' expr (locA lexpr)
enumerateChangesInExpression' expr loc = case expr of
    (ExplicitList ext elems) -> reverse -- Reverse because we started here w/ most specific
        (if length elems == 1
            -- Extract Singleton into item
            then let L _ single = head elems in (
                newChange expr single loc [] Nothing Terminal :
                (rewriteSrc expr <$> enumerateChangesInExpression' single loc)
            )
            else []) ++
        -- Turn a list into a tuple
        (newChange expr (ExplicitTuple EpAnnNotUsed (Present EpAnnNotUsed <$> elems) Boxed) loc [] Nothing Terminal:
        -- Remove element in list
        (splitEverywhere elems
            <&> (\(h, L lremoved removed, t) -> newChange
                expr
                (ExplicitList ext $ h ++ t) -- Removing element in list
                loc
                (enumerateChangesInExpression removed (locA lremoved)
                    <&&> (L lremoved)
                    <&&> (\i -> h ++ [i] ++ t)
                    <&&> (ExplicitList ext))
                Nothing
                Removal
            )
        ))
    (ExplicitTuple _ [Present _ (L lunit unit)] _) -> [
        -- Turn a unit into an item
        -- Note: How to build a 1-tuple ?
        newChange expr unit (locA lunit) [] Nothing Terminal
        ]
    (ExplicitTuple xtuple args box) -> if all tupleArgIsPresent args
            then reverse $ -- Reverse because we started here w/ most specific
                -- Turn a tuple into a list
                newChange expr (ExplicitList EpAnnNotUsed $ mapMaybe getTupleArg args) loc [] Nothing Terminal:
                -- Enumerate each change for each element in the tuple
                concat (splitEverywhere args <&> (\(h, arg, t) -> case arg of
                    Present ext (L lunit unit) -> (enumerateChangesInExpression unit (locA lunit))
                        <&&> (\i -> ExplicitTuple xtuple (h ++ [Present ext (L lunit i)] ++ t) box)
                    _ -> []
                ))
            else []
        where
            tupleArgIsPresent (Present {}) = True
            tupleArgIsPresent _ = False
            getTupleArg (Present _ arg) = Just arg
            getTupleArg _ = Nothing
    -- Attempts tweaks with litterals
    (HsLit ext literal) -> enumerateChangeInLiteral literal loc
        <&&> (HsLit ext)
    -- In function application: try changes on functions and parameters
    (HsApp a func param) -> enumF ++ enumParam
        where
            -- | Enumeration on the function
            enumF = let (L lf f) = func in enumerateChangesInExpression f (locA lf)
                <&&> (\c -> HsApp a (L lf c) param)
            -- | Enumeration on the parameters
            enumParam = let (L lp p) = param in enumerateChangesInExpression p (locA lp)
                <&&> (HsApp a func . L lp)
    -- `let _ = xx in ...` expressions
    (HsLet x bind e) -> enumExpr ++ enumBind
        where
            enumBind = enumerateChangesInLocalBinds bind loc
                <&&> (\newbind -> HsLet x newbind e)
            enumExpr = let (L lexpr letExpr) = e in enumerateChangesInExpression letExpr (locA lexpr)
                <&&> (L lexpr)
                <&&> (HsLet x bind)
    (HsIf ext lifExpr lthenExpr lelseExpr) -> enumIf ++ enumElse ++ enumThen
        where
            enumIf = let (L lif ifExpr) = lifExpr in enumerateChangesInExpression ifExpr (locA lif)
                <&&> (L lif)
                <&&> (\newIf -> HsIf ext newIf lthenExpr lelseExpr)
            enumElse = let (L lelse elseExpr) = lelseExpr in enumerateChangesInExpression elseExpr (locA lelse)
                <&&> (L lelse)
                <&&> (HsIf ext lifExpr lthenExpr)
            enumThen = let (L lthen thenExpr) = lthenExpr in enumerateChangesInExpression thenExpr (locA lthen)
                <&&> (L lthen)
                <&&> (\newthen -> HsIf ext lifExpr newthen lelseExpr)
    (HsCase xcase lrootExpr lmatchExpr) -> enumRoot ++ enumMatches
        where
            enumRoot = let (L lroot root) = lrootExpr in enumerateChangesInExpression root (locA lroot)
                <&&> (L lroot)
                <&&> (\newRoot -> HsCase xcase newRoot lmatchExpr)
            enumMatches = let (MG xmatch (L lmatches matches) origin) = lmatchExpr in concat (splitEverywhere matches
                <&> (\(h, L lmatch match, t) -> enumerateChangesInMatch match (locA lmatch)
                        <&&> (L lmatch)
                        <&&> (\newMatch -> h ++ [newMatch] ++ t)
                        <&&> (L lmatches)
                        <&&> (\newMatches -> MG xmatch newMatches origin)
                        <&&> (HsCase xcase lrootExpr)
                ))
    _ -> []

-- | Expression for `undefined`
undefinedExpression :: HsExpr GhcPs
undefinedExpression = buildFunctionName "undefined"

-- | Build HsExpr (HsVar) from a symbol name
buildFunctionName :: String -> HsExpr GhcPs
buildFunctionName funcName = HsVar noExtField $ L (noAnnSrcSpan noSrcSpan) (mkRdrUnqual (mkVarOcc funcName))

-- | Wraps an expression in parenthesis (AST-wise).
-- Mainly used for pretty printing
wrapExprInPar :: LHsExpr GhcPs -> HsExpr GhcPs
wrapExprInPar = HsPar EpAnnNotUsed


-- | Enumerates changes for a single match
-- See [API doc](https://hackage.haskell.org/package/ghc-9.6.1/docs/Language-Haskell-Syntax-Expr.html#t:Match)
enumerateChangesInMatch :: Enumerator (Match GhcPs (LHsExpr GhcPs))
enumerateChangesInMatch (Match x ctxt pats (GRHSs ext grhss localBinds)) _ = bindingChanges
    where
        -- | Changes for the left-hand side of the `=` symbol
        patChanges = concat $ splitEverywhere pats
            <&> (\(h, L l e, t) -> let (SrcSpanAnn ep loc) = l in (enumerateChangesInPattern e loc
                    <&> wrapLoc (L . SrcSpanAnn ep))
                    <&&> (\r ->  h ++ [r] ++ t)
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

-- | Enumeration of changes for bindings, i.e. anything with an `=`
-- See [API doc](https://hackage.haskell.org/package/ghc-9.6.1/docs/Language-Haskell-Syntax-Binds.html#t:HsBindLR)
enumerateChangesInBinding :: Enumerator (HsBind GhcPs)
enumerateChangesInBinding (FunBind a b c d) l = enumerateChangesInFuncBinding (FunBind a b c d) l
enumerateChangesInBinding (PatBind a (L loc pat) c d) _ = enumerateChangesInPattern pat (locA loc)
    <&&> (L loc)
    <&&> (\b -> PatBind a b c d)
enumerateChangesInBinding (VarBind {}) _ = []
enumerateChangesInBinding (AbsBinds {}) _ = []
enumerateChangesInBinding (PatSynBind {}) _ = []

-- | Enumerates changes to apply on function binding, e.g. `a True = True`.
-- One function binding groups all the matches
-- Basically get changes for each match
enumerateChangesInFuncBinding :: Enumerator (HsBind GhcPs)
enumerateChangesInFuncBinding (FunBind a b (MG c1 (L la ats) c3) d) _ = concat $ splitEverywhere ats
    <&> (\(h, L l e, t) -> let (SrcSpanAnn ep loc) = l in (enumerateChangesInMatch e loc
            <&> wrapLoc (L . SrcSpanAnn ep))
            <&&> (\r ->  h ++ [r] ++ t)
            <&&> (\c2 -> FunBind a b (MG c1 (L la c2) c3) d)
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
                        <&&> (L lbind)
                        <&&> (\r ->  listToBag $ h ++ [r] ++ t)
                        <&&> (\b -> ValBinds xbind b signatures)
                        <&&> (HsValBinds ext)
                )
            enumSignatures = concat $ splitEverywhere signatures
                <&> (\(h, L lsig sign, t) -> enumerateChangeInSignature sign (locA lsig)
                        <&&> (L lsig)
                        <&&> (\r ->  h ++ [r] ++ t)
                        <&&> (HsValBinds ext . ValBinds xbind binds)
                )
enumerateChangesInLocalBinds (HsIPBinds ext implicitbind) l = case implicitbind of
    IPBinds xbind bindlist -> (splitEverywhere bindlist
        <&> (\(h, L lbind bind, t) -> newChange
            bindlist
            (h ++ t) -- Remove bind
            l
            (case bind of
                IPBind bext lname (L lexpr expr) ->
                    enumerateChangesInExpression expr (locA lexpr)
                    <&&> (L lexpr)
                    <&&> (IPBind bext lname)
                    <&&> (L lbind)
                    <&&> (\b -> h ++ [b] ++ t)
                -- _ -> []
            )
            Nothing
            Removal
        ))
        <&&> (HsIPBinds ext . IPBinds xbind)
    -- _ -> []
-- The other cases (`EmptyLocalBinds`, and extensions) do not need to be considered 
enumerateChangesInLocalBinds _ _ = []