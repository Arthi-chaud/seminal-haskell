module Enumerator.Modules(enumerateChangesInModule) where
import GHC (LHsDecl, GhcPs, SrcSpanAnn' (..), HsDecl (ValD), HsBindLR (FunBind), GenLocated (L), HsModule (HsModule, hsmodDecls))
import Changes(Change, newChange, wrapLoc)
import Enumerator.Declarations(enumerateChangesInDeclaration)
import Data.List.HT (splitEverywhere)
import Data.Functor ((<&>))
import Enumerator.Bindings (enumerateChangesInFuncBinding)

enumerateChangesInModule :: HsModule -> [Change HsModule]
enumerateChangesInModule hsmod = case hsmod of
    HsModule {} -> enumerateChangesAtModuleRoot (hsmodDecls hsmod)
        <&> fmap (\decls -> hsmod { hsmodDecls = decls })
    -- We do not need to consider extensions
    -- (From 9.6.x)
    _ -> []

-- | Enumerate changes for the root declaration of a module
enumerateChangesAtModuleRoot :: [LHsDecl GhcPs] -> [Change [LHsDecl GhcPs]]
enumerateChangesAtModuleRoot list = concat $ splitEverywhere list <&> (\(h, L l removed, t) -> let
    (SrcSpanAnn ep removedLoc) = l
    followups = enumerateChangesInDeclaration removed removedLoc
        <&> wrapLoc (L . SrcSpanAnn ep)
        <&> fmap (\r -> h ++ [r] ++ t)
    in case removed of
        (ValD v (FunBind a b c d)) -> enumerateChangesInFuncBinding (FunBind a b c d) removedLoc
            <&> fmap (L l . ValD v)
            <&> fmap (\change -> h ++ [change] ++ t)
        _ -> [newChange list (h ++ t) removedLoc followups]
    )