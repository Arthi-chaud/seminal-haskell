module Seminal.Enumerator.Expressions (
    enumerateChangesInExpression
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
      SrcSpanAnn'(locA),
      MatchGroup(MG),
      GenLocated(L),
      LHsExpr,
      noSrcSpan )
import Seminal.Change
    ( Change(Change, ChangeGroup, src), node,
      ChangeType(Removal),
      ChangeType(Terminal, Wildcard, Wrapping, Removal),
      (<&&>), rewritePretty
    )
import Data.Functor ((<&>))
import Data.List.HT (splitEverywhere)
import GHC.Plugins (mkRdrUnqual, mkVarOcc, Boxity (Boxed), mkDataOcc)
import Seminal.Enumerator.Literals (enumerateChangeInLiteral)
import Data.Maybe (mapMaybe)
import Seminal.Enumerator.LocalBindings (enumerateChangesInLocalBinds)
import {-# SOURCE #-} Seminal.Enumerator.Matches (enumerateChangesInMatch)

-- | Enumerate possible changes for expressions,
-- starting with replacing them with undefined.
-- See [API doc](https://hackage.haskell.org/package/ghc-9.6.1/docs/Language-Haskell-Syntax-Expr.html#t:HsExpr)
enumerateChangesInExpression :: Enumerator (HsExpr GhcPs)
enumerateChangesInExpression expr loc = [change]
    where
        change = ChangeGroup
            (node expr)
            (node <$> [undefinedExpression, ExplicitList EpAnnNotUsed []])
            loc
            (changeToString:changeToList:changeToTrue:subchanges)
            Nothing Wildcard
        -- | Wrap the expression into a list
        changeToList = Change (node expr) (node $ ExplicitList EpAnnNotUsed [lexpr]) loc [] Nothing Wrapping
        -- | Try to call `show` on the Expression
        changeToString = Change (node expr) (node $ HsApp EpAnnNotUsed (locMe $ buildFunctionName "show") lexpr) loc [] Nothing Wrapping
            <&> (wrapExprInPar . locMe)
        changeToTrue = Change (node expr) (node $ HsVar noExtField ltrue) loc [] message Wildcard
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
                Change (node expr) (node single) loc [] Nothing Terminal :
                (enumerateChangesInExpression' single loc
                    <&> (\c -> c { src = rewritePretty expr (src c) })
                )
            )
            else []) ++
        -- Turn a list into a tuple
        (Change (node expr) (node $ ExplicitTuple EpAnnNotUsed (Present EpAnnNotUsed <$> elems) Boxed) loc [] Nothing Terminal:
        -- Remove element in list
        (splitEverywhere elems
            <&> (\(h, L lremoved removed, t) -> Change
                (node expr)
                (node $ ExplicitList ext $ h ++ t) -- Removing element in list
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
        Change (node expr) (node unit) (locA lunit) [] Nothing Terminal
        ]
    (ExplicitTuple xtuple args box) -> if all tupleArgIsPresent args
            then reverse $ -- Reverse because we started here w/ most specific
                -- Turn a tuple into a list
                Change (node expr) (node $ ExplicitList EpAnnNotUsed $ mapMaybe getTupleArg args) loc [] Nothing Terminal:
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
