module Seminal.Enumerator.Types (enumerateChangeInType) where
import GHC (GhcPs, GenLocated (L), HsType (HsWildCardTy, HsTyVar, HsTupleTy, HsAppTy), NoExtField (NoExtField), RdrName, EpAnn (EpAnnNotUsed), HsTupleSort (HsBoxedOrConstraintTuple), noLocA)
import Seminal.Enumerator.Enumerator (Enumerator)
import Seminal.Change (ChangeType(..), node, Change (Change))
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
            (printf "Expected Type `%s`, got `%s`." (showPprUnsafe newType) (showPprUnsafe oldtype)) Terminal
            )
    -- e.g. Maybe a
    (HsAppTy x (L lparent tparent) child) -> monadSubstitutions ++ [removeParent]
        where
            L _ tchild = child
            filteredMonads = filter ((ppr tparent /=) . ppr) topMonads
            monadSubstitutions = buildType <$> filteredMonads <&> (\newM -> Change
                (node typ) [node $ HsAppTy x (L lparent newM) child] loc []
                (printf "Expected `%s`, got `%s`." (showPprUnsafe newM) (showPprUnsafe tparent))
                Terminal
                )
            removeParent = Change
                (node typ) [node tchild] loc []
                (printf "Expected Type `%s`, got `%s`. Maybe you forgot to use `return`?" (showPprUnsafe tchild) (showPprUnsafe typ))
                Terminal
    _  -> []

atomicTypes :: [RdrName]
atomicTypes = (mkRdrUnqual . mkTcOcc) <$> [
    "Int",
    "Integer",
    "Char",
    "String",
    "Bool",
    "Float",
    "Double"
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