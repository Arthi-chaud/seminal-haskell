module Seminal.Enumerator.Types (enumerateChangeInType) where
import GHC (GhcPs, GenLocated (L), HsType (HsWildCardTy, HsTyVar, HsTupleTy, HsAppTy), NoExtField (NoExtField), RdrName, EpAnn (EpAnnNotUsed), HsTupleSort (HsBoxedOrConstraintTuple))
import Seminal.Enumerator.Enumerator (Enumerator)
import Seminal.Change (ChangeType(..), node, Change (Change))
import GHC.Plugins (mkRdrUnqual, showPprUnsafe, mkTcOcc)
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
    (HsAppTy _ tparent (L _ tchild)) -> [
        Change (node typ) [node tchild] loc []
        (printf "Expected Type `%s`, got `%s`. Maybe you forgot to use `return`?" (showPprUnsafe tparent) (showPprUnsafe tchild)) Terminal
        ]
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

unitType :: HsType GhcPs
unitType = HsTupleTy EpAnnNotUsed  HsBoxedOrConstraintTuple []