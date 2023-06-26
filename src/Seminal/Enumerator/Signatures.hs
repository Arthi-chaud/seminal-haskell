module Seminal.Enumerator.Signatures (enumerateChangeInSignature) where
import GHC (Sig (TypeSig), GhcPs, HsWildCardBndrs (HsWC), GenLocated (L), HsSigType (HsSig), SrcSpanAnn' (locA))
import Seminal.Enumerator.Enumerator (Enumerator)
import Seminal.Enumerator.Types(enumerateChangeInType)
import Seminal.Change ((<&&>), forceRewrite)
import Data.Functor

enumerateChangeInSignature :: Enumerator (Sig GhcPs)
enumerateChangeInSignature sig _ = case sig of
    (TypeSig xtypesig ids (HsWC xwc (L lbody (HsSig xsig bndrs (L ltype typ))))) -> enumerateChangeInType typ (locA ltype)
        <&&> (TypeSig xtypesig ids. HsWC xwc . L lbody . HsSig xsig bndrs . L ltype)
        <&> forceRewrite
    _ -> []
