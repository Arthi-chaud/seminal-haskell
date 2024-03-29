{-# LANGUAGE CPP #-}
module Seminal.Enumerator.Types (enumerateChangeInType) where
import Seminal.Compiler.API
import Seminal.Enumerator.Enumerator (Enumerator)
import Seminal.Change (ChangeType(..), node, Change (Change, src, message), (<&&>), forceRewrite)
import Data.Functor ((<&>))
import Text.Printf (printf)
import Data.List.HT (splitEverywhere)
import Data.List (permutations)

enumerateChangeInType :: Enumerator (HsType GhcPs)
enumerateChangeInType typ loc = (case typ of
    (HsTyVar {}) -> ((permutations [wildcardType, typ]) <&> (\newPerm ->
            let newFunTy = typeListToHsFunTy newPerm
                message_ = "A Type is missing." in Change
            (node typ) [node newFunTy] loc
            ((enumerateChangeInType' newFunTy loc)
                <&> (\c -> c { src = node typ, message = message_ })
                <&> forceRewrite)
            message_
            Wildcard
        ))
    _ -> []) ++ enumerateChangeInType' typ loc

enumerateChangeInType' :: Enumerator (HsType GhcPs)
enumerateChangeInType' typ loc = ioWrapping : case typ of
    (HsTyVar _ _  (L _ oldtype)) -> let
        filteredAtomicTypes = filter ((not . eqSDoc (ppr oldtype)) . ppr) atomicTypes
        in (filteredAtomicTypes <&> (\newType ->
            Change (node typ) [node newType] loc []
            (formatMessage newType oldtype) Terminal
            ))
    -- e.g. (Either a) b
    (HsAppTy x lparent lchild) -> (removeParent ++ monadSubstitutions ++ childEnumeration ++ swapChildren) <&> forceRewrite
        where
            (L lp parent) = lparent
            (L _ child) = lchild
            filteredMonads = filter ((not . eqSDoc (ppr monad)) . ppr) topMonads
            monadSubstitutions = buildType <$> filteredMonads <&> (\newM -> Change
                (node typ) [node $ HsAppTy x (L lp newM) lchild] loc []
                (formatMessage newM parent)
                Terminal
                )
            childEnumeration = concat $ splitEverywhere childrenTypes
                <&> (\(h, childType, t) -> enumerateChangeInType' childType loc
                    <&&> (\newChild -> (monad:h ++ newChild:t))
                    <&&> typeListToHsAppTy
                    )
            -- Remove parent only of there is only one child. E.g. yes for IO, no for Either
            removeParent = [
                Change (node typ) [node child] loc [] ((formatMessage child typ) ++ " Maybe you forgot to use `return`?") Terminal
                 | length childrenTypes == 1]
            swapChildren = if length childrenTypes > 1
                then (monad:) <$> permutations childrenTypes <&> (\permutation -> Change
                    (node typ)
                    [node $ typeListToHsAppTy permutation] loc []
                    "Wrong order of Type Argument." Terminal
                )
                else []
            monad = head typeList
            childrenTypes = tail typeList
            typeList = (hsAppTyToList parent) ++ [child]
    -- ()
    (HsTupleTy _ _ []) -> atomicTypes <&> (\newType ->
        Change (node typ) [node newType] loc []
        (formatMessage newType typ) Terminal
        )
    (HsListTy xlist (L lchild child)) -> bracketsRemoval : childEnumeration
        where
        bracketsRemoval = Change (node typ) [node child] loc []
            ((formatMessage child typ) ++ " Maybe you forgot to remove the brackets.")
            Wrapping
        childEnumeration = enumerateChangeInType' child (locA lchild)
            <&&> (HsListTy xlist . L lchild)
            <&> forceRewrite
    (HsParTy x (L l child)) -> enumerateChangeInType' child (locA l)
        <&&> (HsParTy x . L l)
        <&> forceRewrite
    -- e.g. a -> b -> c
    -- Left will be `a`
    -- Right will be b -> c
    (HsFunTy {}) -> removals ++ insertions ++ swaps ++ childEnumerations
        where
            insertions = splitEverywhere typeList
                <&> (\(h, child, t) -> Change
                    (node typ)
                    [node $ typeListToHsFunTy (h ++ [child, wildcardType] ++ t)] loc
                    (atomicTypes <&> (\newType -> Change
                        (node typ) [node $ typeListToHsFunTy (h ++ [child, newType] ++ t)] loc []
                        (printf "A Type is missing: %s" (showPprUnsafe newType))
                        Terminal
                    ))
                    "A Type is missing." Terminal
                )
            childEnumerations = concat $ splitEverywhere typeList
                <&> (\(h, child, t) -> enumerateChangeInType' child loc
                    <&&> (\newChild -> typeListToHsFunTy $ h ++ [newChild] ++ t))
            -- We have to remove duplicates. There is no need to run swpas on `a -> a`
            swaps = let filteredSwaps = filter ((not . eqSDoc (ppr typeList)) . ppr) (permutations typeList) in
                filteredSwaps <&> (\newType -> Change
                    (node typ)
                    [node $ typeListToHsFunTy newType] loc []
                    "The order of the types is wrong" Terminal
                )
            removals = splitEverywhere typeList <&> (\(h, _, t) -> Change
                (node typ)
                [node $ typeListToHsFunTy (h ++ t)] loc []
                "The removed Type is superfluous. Please, remove it." Terminal
                )
            typeList = hsFunTyToList typ
    (HsWildCardTy _) -> (atomicTypes <&> (\newType ->
            Change (node typ) [node newType] loc []
            (formatMessage newType typ) Terminal
        ))
    _ -> []
    where
        ioWrapping = Change
            (node typ)
            [node (HsAppTy NoExtField (noLocA $ buildTypeStr "IO") (noLocA typ))]
            loc []
            "The type was not wrapped with IO." Terminal

atomicTypes :: [HsType GhcPs]
atomicTypes = unitType : (buildTypeStr <$> [
    "Double",
    "Float",
    "Integer",
    "Int",
    "Char",
    "String",
    "Bool"
    ])

topMonads :: [RdrName]
topMonads = (mkRdrUnqual . mkTcOcc) <$> [
    "IO",
    "Maybe"
    ]

unitType :: HsType GhcPs
unitType = HsTupleTy EpAnnNotUsed  HsBoxedOrConstraintTuple []

buildType :: RdrName -> HsType GhcPs
buildType = HsTyVar EpAnnNotUsed NotPromoted . noLocA

buildTypeStr :: String -> HsType GhcPs
buildTypeStr = HsTyVar EpAnnNotUsed NotPromoted . noLocA . mkRdrUnqual . mkTcOcc

formatMessage :: Outputable a => Outputable b => a -> b -> String
formatMessage expected got = printf
    "Expected Type `%s`, got `%s`."
    (showPprUnsafe expected) (showPprUnsafe got)

-- | Turns an HsAppTy into a list of HsType.
-- `Either a b` -> [Either, a, b]
hsAppTyToList :: (HsType GhcPs) -> [HsType GhcPs]
hsAppTyToList (HsAppTy _ (L _ func) (L _ param)) = hsAppTyToList func ++ [param]
hsAppTyToList e = [e]

-- | Turns a list of types into a type application.
-- [Either, a, b] -> `cons 1 2`
typeListToHsAppTy :: [HsType GhcPs] -> (HsType GhcPs)
typeListToHsAppTy [] = undefined
typeListToHsAppTy [e] = e
typeListToHsAppTy [f, p] = HsAppTy NoExtField (reLocA $ noLoc f) (reLocA $ noLoc p)
typeListToHsAppTy list = HsAppTy NoExtField (reLocA $ noLoc $ typeListToHsAppTy (init list)) (reLocA $ noLoc $ last list)

-- | Turns an HsFunTyp into a list of HsType.
-- `a -> b -> c` -> [a, b, c]
hsFunTyToList :: (HsType GhcPs) -> [HsType GhcPs]
hsFunTyToList (HsFunTy _ _ (L _ left) (L _ right)) = left:hsFunTyToList right
hsFunTyToList e = [e]

wildcardType :: HsType GhcPs
wildcardType = HsWildCardTy NoExtField

-- | Turns a list of types into a type.
-- [a, b, c] -> `a -> b -> c`
typeListToHsFunTy :: [HsType GhcPs] -> (HsType GhcPs)
typeListToHsFunTy [] = undefined
typeListToHsFunTy [e] = e
typeListToHsFunTy (left:right) = HsFunTy EpAnnNotUsed
#if MIN_VERSION_ghc(9,4,1)
    (HsUnrestrictedArrow $ L NoTokenLoc HsUnicodeTok)
#else
    (HsUnrestrictedArrow UnicodeSyntax)
#endif
    (noLocA left) (noLocA (typeListToHsFunTy right))