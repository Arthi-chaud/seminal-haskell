module Seminal.Enumerator.Types (enumerateChangeInType) where
import GHC (GhcPs, GenLocated (L), HsType (HsWildCardTy, HsTyVar, HsTupleTy, HsAppTy), NoExtField (NoExtField), RdrName, EpAnn (EpAnnNotUsed), HsTupleSort (HsBoxedOrConstraintTuple), noLocA)
import Seminal.Enumerator.Enumerator (Enumerator)
import Seminal.Change (ChangeType(..), node, Change (Change), (<&&>), forceRewrite)
import GHC.Plugins (mkRdrUnqual, showPprUnsafe, mkTcOcc, Outputable (ppr), PromotionFlag (NotPromoted))
import Data.Functor ((<&>))
import Text.Printf (printf)

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
    -- e.g. Maybe a
    (HsAppTy x parent child) -> monadSubstitutions ++ [removeParent] ++ childEnumeration
        where
            L lparent tparent = parent
            L lchild tchild = child
            filteredMonads = filter ((ppr tparent /=) . ppr) topMonads
            monadSubstitutions = buildType <$> filteredMonads <&> (\newM -> Change
                (node typ) [node $ HsAppTy x (L lparent newM) child] loc []
                (formatMessage newM tparent)
                Terminal
                )
            childEnumeration = enumerateChangeInType tchild loc
                <&&> (HsAppTy x parent . L lchild)
                <&> forceRewrite
            removeParent = Change
                (node typ) [node tchild] loc []
               ((formatMessage tchild typ) ++ " Maybe you forgot to use `return`?")
                Terminal
    (HsTupleTy _ _ []) -> buildType <$> atomicTypes <&> (\newType ->
        Change (node typ) [node newType] loc []
        (formatMessage newType typ) Terminal
        )
    _  -> []

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