module Seminal.Enumerator.LocalBindings (
    enumerateChangesInLocalBinds
) where
import Seminal.Enumerator.Enumerator (Enumerator)
import GHC
    ( GhcPs,
      GenLocated(..),
      SrcSpanAnn'(locA),
      GhcPs,
      SrcSpanAnn'(locA),
      GenLocated(L),
      HsLocalBinds,
      HsLocalBindsLR(HsValBinds, HsIPBinds),
      HsValBindsLR(XValBindsLR, ValBinds),
      HsIPBinds(IPBinds),
      IPBind(IPBind) )
import Seminal.Change
    ( node,
      ChangeType(Removal),
      Change(Change),
      (<&&>)
    )
import Data.Functor ((<&>))
import Data.List.HT (splitEverywhere)
import GHC.Data.Bag (bagToList, listToBag)
import Seminal.Enumerator.Signatures (enumerateChangeInSignature)
import {-# SOURCE #-} Seminal.Enumerator.Expressions (enumerateChangesInExpression)
import Seminal.Enumerator.Bindings (enumerateChangesInBinding)

-- | Enumeration of changes for local bindings, e.g. in a `let` or `where` clause
-- See [API doc](https://hackage.haskell.org/package/ghc-9.6.1/docs/Language-Haskell-Syntax-Binds.html#t:HsLocalBindsLR)
enumerateChangesInLocalBinds :: Enumerator (HsLocalBinds GhcPs)
enumerateChangesInLocalBinds (HsValBinds ext valbind) _ = case valbind of
    XValBindsLR {} -> []
    (ValBinds xbind binds signatures) -> enumSignatures ++ enumBinds
        where
            enumBinds = concat $ splitEverywhere (bagToList binds)
                <&> (\(h, L lbind bind, t) -> enumerateChangesInBinding bind (locA lbind)
                        <&&> (L lbind)
                        <&&> (\r ->  listToBag $ h ++ [r] ++ t)
                        <&&> (\b -> ValBinds xbind b signatures)
                        <&&> (HsValBinds ext)
                )
            enumSignatures = concat $ splitEverywhere signatures
                <&> (\(h, L lsig sign, t) -> enumerateChangeInSignature sign (locA lsig)
                        <&&> (L lsig)
                        <&&> (\r ->  h ++ [r] ++ t)
                        <&&> (HsValBinds ext . ValBinds xbind binds)
                )
enumerateChangesInLocalBinds (HsIPBinds ext implicitbind) l = case implicitbind of
    IPBinds xbind bindlist -> (splitEverywhere bindlist
        <&> (\(h, L lbind bind, t) -> Change
            (node bindlist)
            [node $ h ++ t] -- Remove bind
            l
            (case bind of
                IPBind bext lname (L lexpr expr) ->
                    enumerateChangesInExpression expr (locA lexpr)
                    <&&> (L lexpr)
                    <&&> (IPBind bext lname)
                    <&&> (L lbind)
                    <&&> (\b -> h ++ [b] ++ t)
                -- _ -> []
            )
            Nothing
            Removal
        ))
        <&&> (HsIPBinds ext . IPBinds xbind)
    -- _ -> []
-- The other cases (`EmptyLocalBinds`, and extensions) do not need to be considered 
enumerateChangesInLocalBinds _ _ = []