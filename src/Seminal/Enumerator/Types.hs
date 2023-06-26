module Seminal.Enumerator.Types (enumerateChangeInType) where
import GHC (GhcPs, GenLocated (L), HsType (HsWildCardTy, HsTyVar, HsTupleTy, HsAppTy, HsListTy, HsParTy, HsFunTy), NoExtField (NoExtField), RdrName, EpAnn (EpAnnNotUsed), HsTupleSort (HsBoxedOrConstraintTuple), noLocA, SrcSpanAnn' (locA), noLoc, reLocA)
import Seminal.Enumerator.Enumerator (Enumerator)
import Seminal.Change (ChangeType(..), node, Change (Change), (<&&>), forceRewrite)
import GHC.Plugins (mkRdrUnqual, showPprUnsafe, mkTcOcc, Outputable (ppr), PromotionFlag (NotPromoted))
import Data.Functor ((<&>))
import Text.Printf (printf)
import Data.List.HT (splitEverywhere)
import Data.List (permutations)

enumerateChangeInType :: Enumerator (HsType GhcPs)
enumerateChangeInType typ loc = [
    Change (node typ) [node $ HsWildCardTy NoExtField] loc (enumerateChangeInType' typ loc) "The type is incorrect." Wildcard
    ]

enumerateChangeInType' :: Enumerator (HsType GhcPs)
enumerateChangeInType' typ loc = case typ of
    (HsTyVar xvar pflag (L l oldtype)) -> let
        filteredAtomicTypes = filter (oldtype /=) atomicTypes
        raisedAtomicTypes = (HsTyVar xvar pflag . L l) <$> filteredAtomicTypes
        substitutions = unitType:raisedAtomicTypes
        in substitutions <&> (\newType ->
            Change (node typ) [node newType] loc []
            (formatMessage newType oldtype) Terminal
            )
    -- e.g. (Either a) b
    (HsAppTy x lparent lchild) -> (removeParent ++ monadSubstitutions ++ childEnumeration ++ swapChildren) <&> forceRewrite
        where
            (L lp parent) = lparent
            (L _ child) = lchild
            filteredMonads = filter ((ppr monad /=) . ppr) topMonads
            monadSubstitutions = buildType <$> filteredMonads <&> (\newM -> Change
                (node typ) [node $ HsAppTy x (L lp newM) lchild] loc []
                (formatMessage newM parent)
                Terminal
                )
            childEnumeration = concat $ splitEverywhere childrenTypes
                <&> (\(h, childType, t) -> enumerateChangeInType childType loc
                    <&&> (\newChild -> (monad:h ++ newChild:t))
                    <&&> exprListToHsAppTy
                    )
            -- Remove parent only of there is only one child. E.g. yes for IO, no for Either
            removeParent = [
                Change (node typ) [node child] loc [] ((formatMessage child typ) ++ " Maybe you forgot to use `return`?") Terminal
                 | length childrenTypes == 1]
            swapChildren = if length childrenTypes > 1
                then (monad:) <$> permutations childrenTypes <&> (\permutation -> Change
                    (node typ)
                    [node $ exprListToHsAppTy permutation] loc []
                    "Wrong order of Type Argument." Terminal
                )
                else []
            monad = head typeList
            childrenTypes = tail typeList
            typeList = (hsAppTyToList parent) ++ [child]
    -- ()
    (HsTupleTy _ _ []) -> buildType <$> atomicTypes <&> (\newType ->
        Change (node typ) [node newType] loc []
        (formatMessage newType typ) Terminal
        )
    (HsListTy xlist (L lchild child)) -> bracketsRemoval : childEnumeration
        where
        bracketsRemoval = Change (node typ) [node child] loc []
            ((formatMessage child typ) ++ " Maybe you forgot to remove the brackets.")
            Wrapping
        childEnumeration = enumerateChangeInType child (locA lchild)
            <&&> (HsListTy xlist . L lchild)
            <&> forceRewrite
    (HsParTy x (L l child)) -> enumerateChangeInType child (locA l)
        <&&> (HsParTy x . L l)
        <&> forceRewrite
    -- e.g. a -> b -> c
    (HsFunTy x arrow lleft lright) -> (removals ++ enumSecondType ++ enumFirstType) <&> forceRewrite
        where
            (L ll left) = lleft
            (L lr right) = lright
            enumFirstType = enumerateChangeInType left (locA ll)
                <&&> (\newLeft -> HsFunTy x arrow (L ll newLeft) lright)
            enumSecondType = enumerateChangeInType right (locA lr)
                <&&> (HsFunTy x arrow lleft . L lr)
            removals = [left, right] <&> (\c -> Change
                (node typ)
                [node c]
                loc
                []
                "The removed Type is superfluous. Please, remove it."
                Terminal)
    _ -> []

atomicTypes :: [RdrName]
atomicTypes = (mkRdrUnqual . mkTcOcc) <$> [
    "Double",
    "Float",
    "Integer",
    "Int",
    "Char",
    "String",
    "Bool"
    ]

topMonads :: [RdrName]
topMonads = (mkRdrUnqual . mkTcOcc) <$> [
    "IO",
    "Maybe"
    ]

unitType :: HsType GhcPs
unitType = HsTupleTy EpAnnNotUsed  HsBoxedOrConstraintTuple []

buildType :: RdrName -> HsType GhcPs
buildType = HsTyVar EpAnnNotUsed NotPromoted . noLocA

formatMessage :: Outputable a => Outputable b => a -> b -> String
formatMessage expected got = printf
    "Expected Type `%s`, got `%s`."
    (showPprUnsafe expected) (showPprUnsafe got)

-- | Turns an HsAppTy into a list of HsType.
-- `Either a b` -> [Either, a, b]
hsAppTyToList :: (HsType GhcPs) -> [HsType GhcPs]
hsAppTyToList (HsAppTy _ (L _ func) (L _ param)) = hsAppTyToList func ++ [param]
hsAppTyToList e = [e]

-- | Turns a list of expressions into a function application.
-- [Either, a, b] -> `cons 1 2`
exprListToHsAppTy :: [HsType GhcPs] -> (HsType GhcPs)
exprListToHsAppTy [] = undefined
exprListToHsAppTy [e] = e
exprListToHsAppTy [f, p] = HsAppTy NoExtField (reLocA $ noLoc f) (reLocA $ noLoc p)
exprListToHsAppTy list = HsAppTy NoExtField (reLocA $ noLoc $ exprListToHsAppTy (init list)) (reLocA $ noLoc $ last list)
