module Enumerator.Expressions (enumerateChangesInExpression) where
import Enumerator.Enumerator (Enumerator)
import GHC (
    HsExpr (..),
    GhcPs,
    GenLocated (..),
    noExtField,
    noSrcSpan,
    noAnnSrcSpan,
    EpAnn (EpAnnNotUsed),
    HsTupArg (Present),
    unLoc, noSrcSpanA, SrcSpanAnn' (locA)
    )
import Changes (newChange)
import GHC.Plugins (mkRdrUnqual, mkVarOcc, Boxity (Boxed))
import Enumerator.Literals (enumerateChangeInLiteral)
import Data.Functor ((<&>))

-- | Enumerate possible changes for expressions,
-- starting with replacing them with undefined.
-- See [API doc](https://hackage.haskell.org/package/ghc-9.6.1/docs/Language-Haskell-Syntax-Expr.html#t:HsExpr)
enumerateChangesInExpression :: Enumerator (HsExpr GhcPs)
enumerateChangesInExpression expr loc = [changeToUndefined]
    where
        -- | Change the expression to `undefined`, used as a wildcard
        changeToUndefined = newChange expr undefinedExpression loc (changeToList:subchanges)
        -- | Wrap the expression into a list
        changeToList = newChange expr (ExplicitList EpAnnNotUsed [L noSrcSpanA expr]) loc []
        -- | The other, specalised, changes to consider
        subchanges = enumerateChangesInExpression' expr loc

enumerateChangesInExpression' :: Enumerator (HsExpr GhcPs)
enumerateChangesInExpression' expr loc =  case expr of
    (ExplicitList _ [a]) -> [
        -- Extract singleton into an item
        newChange expr (unLoc a) loc []
        ]
    (ExplicitList _ elems) -> [
        -- Turn a list into a tuple
        newChange expr (ExplicitTuple EpAnnNotUsed (Present EpAnnNotUsed <$> elems) Boxed) loc []
        ]
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
undefinedExpression = HsVar noExtField $ L (noAnnSrcSpan noSrcSpan) (mkRdrUnqual (mkVarOcc "undefined"))
