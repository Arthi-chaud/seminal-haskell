module Enumerator.Bindings (enumerateChangesInBinding, enumerateChangesInFuncBinding) where

import Enumerator.Matches(enumerateChangesInMatch)
import Enumerator.Enumerator(Enumerator) 
import GHC (
    GhcPs,
    HsBindLR (..),
    SrcSpanAnn' (SrcSpanAnn, locA),
    MatchGroup (MG),
    HsBind,
    GenLocated (L)
    )
import Change (wrapLoc)
import Enumerator.Patterns (enumerateChangesInPattern)
import Data.Functor ((<&>))
import Data.List.HT (splitEverywhere)

-- | Enumeration of changes for bindings, i.e. anything with an `=`
-- See [API doc](https://hackage.haskell.org/package/ghc-9.6.1/docs/Language-Haskell-Syntax-Binds.html#t:HsBindLR)
enumerateChangesInBinding :: Enumerator (HsBind GhcPs)
enumerateChangesInBinding (FunBind a b c d) l = enumerateChangesInFuncBinding (FunBind a b c d) l
enumerateChangesInBinding (PatBind a (L loc pat) c d) _ = enumerateChangesInPattern pat (locA loc)
    <&> fmap (L loc)
    <&> fmap (\b -> PatBind a b c d)
enumerateChangesInBinding (VarBind _ _ _) loc = []
enumerateChangesInBinding (AbsBinds {}) loc = []
enumerateChangesInBinding (PatSynBind {}) loc = []

-- | Enumerates changes to apply on function binding, e.g. `a True = True`.
-- One function binding groups all the matches
-- Basically get changes for each match
enumerateChangesInFuncBinding :: Enumerator (HsBind GhcPs)
enumerateChangesInFuncBinding (FunBind a b (MG c1 (L la ats) c3) d) _ = concat $ splitEverywhere ats
    <&> (\(h, L l e, t) -> let (SrcSpanAnn ep loc) = l in enumerateChangesInMatch e loc
            <&> wrapLoc (L . SrcSpanAnn ep)
            <&> fmap (\r ->  h ++ [r] ++ t)
            <&> fmap (\c2 -> FunBind a b (MG c1 (L la c2) c3) d)
    )
enumerateChangesInFuncBinding _ _ = []
