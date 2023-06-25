module Seminal.Enumerator.Signatures (enumerateChangeInSignature) where
import GHC (Sig (TypeSig), GhcPs, HsWildCardBndrs (HsWC), GenLocated (L), HsSigType (HsSig), HsType (HsWildCardTy, HsTyVar), NoExtField (NoExtField), SrcSpanAnn' (locA), RdrName)
import Seminal.Enumerator.Enumerator (Enumerator)
import Seminal.Change (ChangeType(..), node, Change (Change), (<&&>))
import GHC.Plugins (mkRdrUnqual, showPprUnsafe, mkTcOcc)
import Data.Functor ((<&>))
import Text.Printf (printf)

enumerateChangeInSignature :: Enumerator (Sig GhcPs)
enumerateChangeInSignature sig _ = case sig of
    (TypeSig xtypesig ids (HsWC xwc (L lbody (HsSig xsig bndrs (L ltype typ))))) -> enumerateChangeInType typ (locA ltype)
        <&&> (TypeSig xtypesig ids. HsWC xwc . L lbody . HsSig xsig bndrs . L ltype)
    _ -> []

enumerateChangeInType :: Enumerator (HsType GhcPs)
enumerateChangeInType typ loc = [
    Change (node typ) [node $ HsWildCardTy NoExtField] loc (enumerateChangeInType' typ loc) "The type is incorrect." Wildcard
    ]

enumerateChangeInType' :: Enumerator (HsType GhcPs)
enumerateChangeInType' typ loc = case typ of
    (HsTyVar xvar pflag (L l oldtype)) -> (filter (oldtype /=)) atomicTypes <&> (\newType ->
        Change (node typ) [node $ HsTyVar xvar pflag $ L l newType] loc []
        (printf "Expected Type `%s`, got `%s`." (showPprUnsafe newType) (showPprUnsafe oldtype)) Terminal
        )
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