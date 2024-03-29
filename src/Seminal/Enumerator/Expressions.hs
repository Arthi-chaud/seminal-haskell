{-# LANGUAGE CPP #-}
module Seminal.Enumerator.Expressions (
    enumerateChangesInExpression
) where
import Seminal.Enumerator.Enumerator (Enumerator)
import Seminal.Compiler.API
import Seminal.Change
import Data.Functor ((<&>))
import Data.List.HT (splitEverywhere)
import Seminal.Enumerator.Literals (enumerateChangeInLiteral, enumerateRaiseInLiterals)
import Data.Maybe (mapMaybe)
import Seminal.Enumerator.LocalBindings (enumerateChangesInLocalBinds)
import {-# SOURCE #-} Seminal.Enumerator.Matches (enumerateChangesInMatch)
import Data.List (permutations)

-- | Enumerate possible changes for expressions,
-- starting with replacing them with undefined.
-- See [API doc](https://hackage.haskell.org/package/ghc-9.6.1/docs/Language-Haskell-Syntax-Expr.html#t:HsExpr)
enumerateChangesInExpression :: Enumerator (HsExpr GhcPs)
enumerateChangesInExpression expr loc = [change]
    where
        change = Change
            (node expr)
            (node <$> [undefinedExpression, emptyListExpression])
            loc
            (changeToString:changeToList:changeToTrue:changeToUnit:subchanges)
            "The type of the expression is incorrect."
            Wildcard
        changeToUnit = Change (node expr) [node unitExpression] loc []
            "The expected type of the expression is `()`." Terminal
        -- | Wrap the expression into a list
        changeToList = Change (node expr) [node $ ExplicitList EpAnnNotUsed [lexpr]] loc []
            "The expected type of the expression is a list. You may have forgotten to wrap the expression into a list" Wrapping
        -- | Try to call `show` on the Expression
        changeToString = Change (node expr) [node $ HsApp EpAnnNotUsed (locMe $ buildFunctionName "show") lexpr] loc []
            "The expected type of the expression is a String. You may have forgotten to call the `show` method on the expression" Wrapping
            <&> (wrapExprInPar . locMe)
        changeToTrue = Change (node expr) [node $ HsVar NoExtField ltrue] loc [] msg Wildcard
            where
                msg = "This expression might need to evaluate to a boolean value. It is not the case here. Check the type of the value."
                ltrue = L (noAnnSrcSpan noSrcSpan) (mkRdrUnqual (mkDataOcc "True"))
        -- | The other, specalised, changes to consider
        subchanges = enumerateChangesInExpression' expr loc
        -- | Located expr
        lexpr = locMe expr
        -- | Wrap HsExpr into LHsExpr
        locMe = L noSrcSpanA

enumerateChangesInExpression' :: Enumerator (HsExpr GhcPs)
#if MIN_VERSION_ghc(9,4,1)
enumerateChangesInExpression' (HsPar _ _ (L lexpr expr) _) _ = 
#else
enumerateChangesInExpression' (HsPar _ (L lexpr expr)) _ =
#endif
    enumerateChangesInExpression' expr (locA lexpr)
enumerateChangesInExpression' expr loc = case expr of
    (ExplicitList ext elems) -> reverse -- Reverse because we started here w/ most specific
        (if length elems == 1
            -- Extract Singleton into item
            then let L _ single = head elems in (
                Change (node expr) [node single] loc []
                "The expected type of the expression is not a list of the given expression. You may need to remove brackets around it." Terminal :
                -- We rewrite the source, because it is the list, not the element inside it
                (enumerateChangesInExpression' single loc <&> (\c -> c { src = node expr }))
            )
            else []) ++
        -- Turn a list into a tuple
        (Change (node expr) [node $ ExplicitTuple EpAnnNotUsed (Present EpAnnNotUsed <$> elems) Boxed] loc []
            "The expected type of the expression is a tuple, not a list." Terminal:
        -- Remove element in list
        (splitEverywhere elems
            <&> (\(h, L lremoved removed, t) -> Change
                (node expr)
                [node $ ExplicitList ext $ h ++ t] -- Removing element in list
                loc
                (enumerateChangesInExpression removed (locA lremoved)
                    <&&> (L lremoved)
                    <&&> (\i -> h ++ [i] ++ t)
                    <&&> (ExplicitList ext))
                "The removed element is either of the wrong type, or its sub-expression does not type-check."
                Removal
            )
        ))
    (ExplicitTuple _ [Present _ (L lunit unit)] _) -> [
        -- Turn a unit into an item
        -- Note: How to build a 1-tuple ?
        Change (node expr) [node unit] (locA lunit) []
            "The expected type of the expression is a list, not a tuple." Terminal
        ]
    (ExplicitTuple xtuple args box) -> if all tupleArgIsPresent args
            then reverse $ -- Reverse because we started here w/ most specific
                -- Turn a tuple into a list
                Change (node expr) [node $ ExplicitList EpAnnNotUsed $ mapMaybe getTupleArg args] loc []
                    "The expected type of the expression is a list, not a tuple." Terminal:
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
        <&&> (HsLit ext) ++ enumerateRaiseInLiterals expr loc
    -- In function application: try changes on functions and parameters
    (HsApp a func param) -> paramInsert ++ paramRemovals ++ enumF ++ enumParam ++ paramSwap
        where
            -- | Enumeration on the function
            enumF = let (L lf f) = func in enumerateChangesInExpression f (locA lf)
                <&&> (\c -> HsApp a (L lf c) param)
            -- | Enumeration on the parameters
            enumParam = let (L lp p) = param in enumerateChangesInExpression p (locA lp)
                <&&> (HsApp a func . L lp)
            paramRemovals = splitEverywhere paramList
                <&> (\(h, _, t) -> Change
                    (node expr)
                    [node (exprListToHsApp (h ++ t))]
                    loc
                    []
                    "The removed expression is superfluous. Please, remove it."
                    Terminal
                )
            -- Swap Parameter
            paramSwap = permutations paramList
                <&> exprListToHsApp
                <&> (\c -> Change
                    (node expr)
                    [node c]
                    loc
                    []
                    "The order of the arguments is invalid."
                    Terminal
                )
            -- Insert Parameter
            paramInsert = splitEverywhere paramList
                <&> (\(h, e, t) -> Change
                    (node expr)
                    [node (exprListToHsApp (h ++ [e, undefinedExpression] ++ t))]
                    loc
                    []
                    "An argument is missing in the function application. Check the number of expected arguments."
                    Addition
                )
            paramList = hsAppToList expr
    -- `let _ = xx in ...` expressions
#if MIN_VERSION_ghc(9,4,1)
    (HsLet x letToken bind inToken e) -> enumExpr ++ enumBind
#else
    (HsLet x bind e) -> enumExpr ++ enumBind
#endif
        where
            enumBind = enumerateChangesInLocalBinds bind loc
#if MIN_VERSION_ghc(9,4,1)
                <&&> (\newbind -> HsLet x letToken newbind inToken e)
#else
                <&&> (\newbind -> HsLet x newbind e)
#endif
            enumExpr = let (L lexpr letExpr) = e in enumerateChangesInExpression letExpr (locA lexpr)
                <&&> (L lexpr)
#if MIN_VERSION_ghc(9,4,1)
                <&&> (HsLet x letToken bind inToken)
#else
                <&&> (HsLet x bind)
#endif
                
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
            enumMatches = let
#if MIN_VERSION_ghc(9,6,1)
                (MG xmatch (L lmatches matches)) = lmatchExpr
#else
                (MG xmatch (L lmatches matches) origin) = lmatchExpr
#endif
                in concat (splitEverywhere matches
                    <&> (\(h, L lmatch match, t) -> enumerateChangesInMatch match (locA lmatch)
                            <&&> (L lmatch)
                            <&&> (\newMatch -> h ++ [newMatch] ++ t)
                            <&&> (L lmatches)
#if MIN_VERSION_ghc(9,6,1)
                            <&&> (MG xmatch)
#else
                            <&&> (\newMatches -> MG xmatch newMatches origin)
#endif
                            <&&> (HsCase xcase lrootExpr)
                    ))
    (OpApp xapp lleftExpr lopExpr lrightExpr) -> enumLeft ++ enumOp ++ enumRight
        where
            enumLeft = let (L lleft leftExpr) = lleftExpr in enumerateChangesInExpression leftExpr (locA lleft)
                <&&> (L lleft)
                <&&> (\newLeft -> OpApp xapp newLeft lopExpr lrightExpr)
            enumRight = let (L lright rightExpr) = lrightExpr in enumerateChangesInExpression rightExpr (locA lright)
                <&&> (L lright)
                <&&> (OpApp xapp lleftExpr lopExpr)
            enumOp = let (L lop opExpr) = lopExpr in enumerateChangesInExpression opExpr (locA lop)
                <&&> (L lop)
                <&&> (\newOp -> OpApp xapp lleftExpr newOp lrightExpr)
    (NegApp xapp lexpr syntaxExpr) -> let (L l e) = lexpr in enumerateChangesInExpression e (locA l)
            <&&> (L l)
            <&&> (\nexExpt -> NegApp xapp nexExpt syntaxExpr)
    (HsDo xdo flavor llstmts) -> let (L ll lstmts) = llstmts in concat (splitEverywhere lstmts <&> (\(h, (L l stmt), t) -> case stmt of
        BodyStmt xbody (L le e) s1 s2 -> (enumerateChangesInExpression e (locA le) ++ (
            -- If it is the last expression in the body, try to call `return`
            [Change (node e) [node (HsApp EpAnnNotUsed (L le $ buildFunctionName "return") (L le e))] (locA le) []
                "The type of the expression is correct, but it was not wrapped in the monad." Terminal | null t]
            ))
            <&&> (L le)
            <&&> (\n -> BodyStmt xbody n s1 s2)
            <&&> (\n -> h ++ [L l n] ++ t)
        _ -> []
        ))
        <&&> (HsDo xdo flavor . L ll)
    _ -> []

-- | Expression for `undefined`
undefinedExpression :: HsExpr GhcPs
undefinedExpression = buildFunctionName "undefined"

-- | Expression for `[]`
emptyListExpression :: HsExpr GhcPs
emptyListExpression = ExplicitList EpAnnNotUsed []

-- | Expression for `()`
unitExpression :: HsExpr GhcPs
unitExpression = ExplicitTuple EpAnnNotUsed [] Boxed

-- | Build HsExpr (HsVar) from a symbol name
buildFunctionName :: String -> HsExpr GhcPs
buildFunctionName funcName = HsVar NoExtField $ L (noAnnSrcSpan noSrcSpan) (mkRdrUnqual (mkVarOcc funcName))

-- | Wraps an expression in parenthesis (AST-wise).
-- Mainly used for pretty printing
wrapExprInPar :: LHsExpr GhcPs -> HsExpr GhcPs
#if MIN_VERSION_ghc(9,4,1)
wrapExprInPar e = HsPar EpAnnNotUsed (L NoTokenLoc HsTok) e (L NoTokenLoc HsTok)
#else
wrapExprInPar = HsPar EpAnnNotUsed
#endif

-- | Turns an HsApp into a list of expression.
-- `const 1 2` -> [cons, 1, 2]
hsAppToList :: (HsExpr GhcPs) -> [HsExpr GhcPs]
hsAppToList (HsApp _ (L _ func) (L _ param)) = hsAppToList func ++ [param]
hsAppToList e = [e]

-- | Turns a list of expressions into a function application.
-- [cons, 1, 2] -> `cons 1 2`
exprListToHsApp :: [HsExpr GhcPs] -> (HsExpr GhcPs)
exprListToHsApp [] = undefined
exprListToHsApp [f, p] = HsApp EpAnnNotUsed (reLocA $ noLoc f) (reLocA $ noLoc p)
exprListToHsApp [e] = e
exprListToHsApp list = HsApp EpAnnNotUsed (reLocA $ noLoc $ exprListToHsApp (init list)) (reLocA $ noLoc $ last list)
