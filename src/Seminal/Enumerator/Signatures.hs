module Seminal.Enumerator.Signatures (enumerateChangeInSignature) where
import Seminal.Compiler.API 
import Seminal.Enumerator.Enumerator (Enumerator)
import Seminal.Enumerator.Types(enumerateChangeInType)
import Seminal.Change ((<&&>), forceRewrite)
import Data.Functor ((<&>))

enumerateChangeInSignature :: Enumerator (Sig GhcPs)
enumerateChangeInSignature sig _ = case sig of
    (TypeSig xtypesig ids (HsWC xwc (L lbody (HsSig xsig bndrs (L ltype typ))))) -> enumerateChangeInType typ (locA ltype)
        <&&> (TypeSig xtypesig ids. HsWC xwc . L lbody . HsSig xsig bndrs . L ltype)
        <&> forceRewrite
    _ -> []
