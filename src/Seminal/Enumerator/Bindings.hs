module Seminal.Enumerator.Bindings (
    enumerateChangesInBinding,
    enumerateChangesInFuncBinding
) where
import Seminal.Enumerator.Enumerator (Enumerator)
import Seminal.Compiler.API
import Seminal.Change ((<&&>))
import Seminal.Enumerator.Patterns (enumerateChangesInPattern)
import Data.Functor ((<&>))
import Data.List.HT (splitEverywhere)
import {-# SOURCE #-} Seminal.Enumerator.Matches (enumerateChangesInMatch)

-- | Enumeration of changes for bindings, i.e. anything with an `=`
-- See [API doc](https://hackage.haskell.org/package/ghc-9.6.1/docs/Language-Haskell-Syntax-Binds.html#t:HsBindLR)
enumerateChangesInBinding :: Enumerator (HsBind GhcPs)
enumerateChangesInBinding (FunBind a b c) l = enumerateChangesInFuncBinding (FunBind a b c) l
enumerateChangesInBinding (PatBind a (L loc pat) c) _ = enumerateChangesInPattern pat (locA loc)
    <&&> (L loc)
    <&&> (\b -> PatBind a b c)
enumerateChangesInBinding (VarBind {}) _ = []
enumerateChangesInBinding (PatSynBind {}) _ = []

-- | Enumerates changes to apply on function binding, e.g. `a True = True`.
-- One function binding groups all the matches
-- Basically get changes for each match
enumerateChangesInFuncBinding :: Enumerator (HsBind GhcPs)
enumerateChangesInFuncBinding (FunBind a b (MG c1 (L la ats))) _ = concat $ splitEverywhere ats
    <&> (\(h, L l e, t) -> let (SrcSpanAnn _ loc) = l in enumerateChangesInMatch e loc
            <&&> (\r ->  h ++ [L l r] ++ t)
            <&&> (FunBind a b . MG c1 . L la)
    )
enumerateChangesInFuncBinding _ _ = []
