{-# LANGUAGE CPP #-}
module Seminal.Enumerator.Bindings (
    enumerateChangesInBinding,
    enumerateChangesInFuncBinding
) where
import Seminal.Compiler.API 
import Seminal.Enumerator.Enumerator (Enumerator)
import Seminal.Change ((<&&>))
import Seminal.Enumerator.Patterns (enumerateChangesInPattern)
import Data.Functor ((<&>))
import Data.List.HT (splitEverywhere)
import {-# SOURCE #-} Seminal.Enumerator.Matches (enumerateChangesInMatch)

-- | Enumeration of changes for bindings, i.e. anything with an `=`
-- See [API doc](https://hackage.haskell.org/package/ghc-9.6.1/docs/Language-Haskell-Syntax-Binds.html#t:HsBindLR)
enumerateChangesInBinding :: Enumerator (HsBind GhcPs)
#if MIN_VERSION_ghc(9,6,1)
enumerateChangesInBinding (FunBind a b c) l = enumerateChangesInFuncBinding (FunBind a b c) l
#else
enumerateChangesInBinding (FunBind a b c d) l = enumerateChangesInFuncBinding (FunBind a b c d) l
#endif
#if MIN_VERSION_ghc(9,6,1)
enumerateChangesInBinding (PatBind a (L loc pat) c) _ =
#else
enumerateChangesInBinding (PatBind a (L loc pat) c d) _ =
#endif
    enumerateChangesInPattern pat (locA loc)
        <&&> (L loc)
#if MIN_VERSION_ghc(9,6,1)
        <&&> (\b -> PatBind a b c)
#else
        <&&> (\b -> PatBind a b c d)
#endif
enumerateChangesInBinding _ _ = []

-- | Enumerates changes to apply on function binding, e.g. `a True = True`.
-- One function binding groups all the matches
-- Basically get changes for each match
enumerateChangesInFuncBinding :: Enumerator (HsBind GhcPs)
#if MIN_VERSION_ghc(9,6,1)
enumerateChangesInFuncBinding (FunBind a b (MG c1 (L la ats))) _ =
#else
enumerateChangesInFuncBinding (FunBind a b (MG c1 (L la ats) c3) d) _ =
#endif
    concat $ splitEverywhere ats
        <&> (\(h, L l e, t) -> let (SrcSpanAnn _ loc) = l in enumerateChangesInMatch e loc
                <&&> (\r ->  h ++ [L l r] ++ t)
#if MIN_VERSION_ghc(9,6,1)
                <&&> (\c2 -> FunBind a b (MG c1 (L la c2)))
#else
                <&&> (\c2 -> FunBind a b (MG c1 (L la c2) c3) d)
#endif
        )
enumerateChangesInFuncBinding _ _ = []
