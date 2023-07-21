-- | Module that imports (either from GHC's API or ghc-lib-parser) and export GHC's API
module Seminal.Compiler.API (
    SrcSpan, GenLocated(..), noSrcSpan, noLoc, unLoc, getLoc,
    realSrcSpan, noAnnSrcSpan, EpAnn(..), noSrcSpanA, noLocA, reLocA, SrcSpanAnn'(..), TokenLocation(..),
    SDoc, Outputable, ppr, showSDocUnsafe, showPprUnsafe,
    HsModule(..),
    Boxity(..),
    LHsExpr, HsExpr(..), Match(..), HsTupArg(..), MatchGroup(..),  StmtLR(..), GRHSs(..), GRHS(..),
    LHsDecl, HsDecl(..), TyClDecl(..),
    OverLitVal(..), HsLit(..), HsOverLit(..),
    HsBind, HsLocalBinds, HsBindLR(..), Sig(..),  HsLocalBindsLR(..), HsValBindsLR(..), HsIPBinds(..), IPBind(..),
    Pat(..),
    HsToken(..), HsUniToken(..),
    HsType(..), HsTupleSort(..), HsArrow(..), HsSigType(..), HsWildCardBndrs(..),
    GhcPs,
    SourceText(NoSourceText), mkIntegralLit, mkTHFractionalLit,
    Ghc,
    ParsedModule(..),
    liftIO,
    mkFastString, unpackFS,
    noExtField, NoExtField(..),
    integerTy,
    PromotionFlag(..),
    RdrName, mkRdrUnqual,
    mkTcOcc, mkVarOcc, mkDataOcc,
    bagToList, listToBag
) where

import "ghc-lib" GHC.Types.SrcLoc (SrcSpan, GenLocated(..), noSrcSpan, noLoc, unLoc, getLoc)
import "ghc-lib" GHC.Parser.Annotation (
    realSrcSpan, noAnnSrcSpan, EpAnn(..),
    noSrcSpanA, noLocA, reLocA, SrcSpanAnn'(..),
    TokenLocation(..)
    )

import "ghc-lib" GHC.Utils.Outputable (
    SDoc, Outputable, ppr, showSDocUnsafe, showPprUnsafe
    )

import "ghc-lib" Language.Haskell.Syntax (HsModule(..))
import "ghc-lib" Language.Haskell.Syntax.Basic (Boxity(..))
import "ghc-lib" Language.Haskell.Syntax.Expr (
    LHsExpr, HsExpr(..), Match(..),
    HsTupArg(..), MatchGroup(..), StmtLR(..),
    GRHSs(..), GRHS(..)
    )
import "ghc-lib" Language.Haskell.Syntax.Decls (LHsDecl, HsDecl(..), TyClDecl(..))
import "ghc-lib" Language.Haskell.Syntax.Lit (OverLitVal(..), HsLit(..), HsOverLit(..))
import "ghc-lib" Language.Haskell.Syntax.Binds (
    HsBind, HsLocalBinds, HsBindLR(..), Sig(..),
    HsLocalBindsLR(..), HsValBindsLR(..), HsIPBinds(..), IPBind(..)
    )
import "ghc-lib" Language.Haskell.Syntax.Pat (Pat(..))
import "ghc-lib" Language.Haskell.Syntax.Concrete (HsToken(..), HsUniToken(..))
import "ghc-lib" Language.Haskell.Syntax.Type (
    HsType(..), HsTupleSort(..), HsArrow(..),
    HsSigType(..), HsWildCardBndrs(..)
    )

import "ghc-lib" GHC(ParsedModule(..))
import "ghc-lib" GHC.Driver.Monad (liftIO)
import "ghc-lib" GHC.Data.FastString (mkFastString, unpackFS)
import "ghc-lib" GHC.Builtin.Types (integerTy)

import "ghc-lib" GHC.Types.SourceText (
    SourceText(NoSourceText), mkIntegralLit, mkTHFractionalLit
    )

import "ghc-lib" GHC.Hs.Extension (GhcPs)

import "ghc-lib" GHC.Driver.Monad (Ghc)

import "ghc-lib" Language.Haskell.Syntax.Extension (noExtField, NoExtField(..))

import "ghc-lib" GHC.Types.Basic(PromotionFlag(..))
import "ghc-lib" GHC.Types.Name.Reader(RdrName, mkRdrUnqual)
import "ghc-lib" GHC.Types.Name.Occurrence(mkTcOcc, mkVarOcc, mkDataOcc)

import "ghc-lib" GHC.Data.Bag (bagToList, listToBag)