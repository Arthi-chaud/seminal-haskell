{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use list comprehension" #-}
module Seminal.Enumerator.Expressions (enumerateChangesInExpression) where
import Seminal.Enumerator.Enumerator (Enumerator)
import GHC (
    HsExpr (..),
    GhcPs,
    GenLocated (..),
    noExtField,
    noSrcSpan,
    noAnnSrcSpan,
    EpAnn (EpAnnNotUsed),
    HsTupArg (Present), noSrcSpanA, SrcSpanAnn' (locA), LHsExpr
    )
import Seminal.Change (newChange, rewriteSrc, ChangeType (Terminal, Wildcard, Wrapping, Removal))
import GHC.Plugins (mkRdrUnqual, mkVarOcc, Boxity (Boxed))
import Seminal.Enumerator.Literals (enumerateChangeInLiteral)
import Data.Functor ((<&>))
import Data.List.HT (splitEverywhere)

-- | Enumerate possible changes for expressions,
-- starting with replacing them with undefined.
-- See [API doc](https://hackage.haskell.org/package/ghc-9.6.1/docs/Language-Haskell-Syntax-Expr.html#t:HsExpr)
enumerateChangesInExpression :: Enumerator (HsExpr GhcPs)
enumerateChangesInExpression expr loc = [changeToUndefined]
    where
        -- | Change the expression to `undefined`, used as a wildcard
        changeToUndefined = newChange expr undefinedExpression loc (changeToList:changeToString:subchanges) Nothing Wildcard
        -- | Wrap the expression into a list
        changeToList = newChange expr (ExplicitList EpAnnNotUsed [lexpr]) loc [] Nothing Wrapping
        -- | Try to call `show` on the Expression
        changeToString = newChange expr (HsApp EpAnnNotUsed (locMe $ buildFunctionName "show") lexpr) loc [] Nothing Wrapping
            <&> (wrapExprInPar . locMe)
        -- | The other, specalised, changes to consider
        subchanges = enumerateChangesInExpression' expr loc
        -- | Located expr
        lexpr = locMe expr
        -- | Wrap HsExpr into LHsExpr
        locMe = L noSrcSpanA

enumerateChangesInExpression' :: Enumerator (HsExpr GhcPs)
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
                    <&> fmap (L lremoved)
                    <&> fmap (\i -> h ++ [i] ++ t)
                    <&> fmap (ExplicitList ext))
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
                newChange expr (ExplicitList EpAnnNotUsed $ getTupleArg <$> args) loc [] Nothing Terminal:
                -- Enumerate each change for each element in the tuple
                concat (splitEverywhere args <&> (\(h, Present ext (L lunit unit), t) ->
                    (enumerateChangesInExpression unit (locA lunit))
                    <&> fmap (\i -> ExplicitTuple xtuple (h ++ [Present ext (L lunit i)] ++ t) box)
                ))
            else []
        where
            tupleArgIsPresent (Present {}) = True
            tupleArgIsPresent _ = False
            getTupleArg (Present _ arg) = arg
    -- Attempts tweaks with litterals
    (HsLit ext literal) -> enumerateChangeInLiteral literal loc
        <&> fmap (HsLit ext)
    -- In function application: try changes on functions and parameters
    (HsApp a func param) -> enumF ++ enumParam
        where
            -- | Enumeration on the function
            enumF = let (L lf f) = func in enumerateChangesInExpression f (locA lf)
                <&> fmap (\c -> HsApp a (L lf c) param)
            -- | Enumeration on the parameters
            enumParam = let (L lp p) = param in enumerateChangesInExpression p (locA lp)
                <&> fmap (HsApp a func . L lp)
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