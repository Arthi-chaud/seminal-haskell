{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use fewer imports" #-}
{-# LANGUAGE PatternSynonyms #-}
-- | We want to split imports as muc has possible to use CPP preprocessors more easily

-- | Import and reexport GHC's API. This is done to make transitions between GHC's version easier
module Seminal.Compiler.API (
    GenLocated(..),
    unLoc,
    getLoc,
    HsModule,
    pattern GHCHsModule.HsModule,
    ParsedModule(..),
    Ghc,
    liftIO,
    SrcSpan,
    GhcPs,
    SrcSpanAnn'(..),
    HsDecl(..),
    TyClDecl(..),
    HsBind,
    HsBindLR(..),
    MatchGroup(..),
    typecheckModule,
    runGhc,
    setSessionDynFlags,
    getSessionDynFlags,
    setTargets,
    guessTarget,
    parseModule,
    depanal,
    load,
    LoadHowMuch(..),
    DynFlags(..),
    mgModSummaries,
    GhcException(..),
    mkModuleName,
    GhcLink(..),
    realSrcSpan,
    mkIntegralLit,
    mkTHFractionalLit,
    SourceText(..),
    unpackFS,
    mkFastString,
    integerTy,
    NoExtField(..),
    HsExpr(..),
    HsLit(..),
    HsOverLit(..),
    OverLitVal(..),
    HsType(..),
    HsArrow(..),
    noLoc,
    noLocA,
    reLocA,
    EpAnn(..),
    HsTupleSort(..),
#if MIN_VERSION_ghc(9,4,1)
    HsToken(..),
    HsUniToken(..),
    TokenLocation(..),
#else
    IsUnicodeSyntax(..),
#endif 
    RdrName,
    showPprUnsafe,
    ppr,
    mkRdrUnqual,
    mkTcOcc,
    PromotionFlag(..),
    Outputable,
    Sig(..),
    HsWildCardBndrs(..),
    HsSigType(..),
    Pat(..),
    HsLocalBinds,
    bagToList,
    listToBag,
    HsLocalBindsLR(..),
    HsValBindsLR(..),
    HsIPBinds(..),
    IPBind(..),
    LHsDecl,
    LHsExpr,
    Match(..),
    noSrcSpan,
    noSrcSpanA,
    GRHSs(..),
    GRHS(..),
    SDoc,
    mkVarOcc,
    mkDataOcc,
    Boxity(..),
    noAnnSrcSpan,
    StmtLR(..),
    HsTupArg(..),
    handleSourceError,
    isContainedIn,
    Extension(..),
    insert,
    throwGhcException,
    msHsFilePath,
    noBackend,
    hsmodDecls,
#if MIN_VERSION_ghc(9,6,1)
    eqSDoc,
#endif
) where

-- | Location
import GHC.Types.SrcLoc(GenLocated(..))
import GHC.Types.SrcLoc(unLoc)
import GHC.Types.SrcLoc(noLoc)
import GHC.Types.SrcLoc(getLoc)
import GHC.Types.SrcLoc(SrcSpan)
import GHC.Types.SrcLoc(noSrcSpan)

-- | Annotations
import GHC.Parser.Annotation(SrcSpanAnn'(..))
import GHC.Parser.Annotation(realSrcSpan)
#if MIN_VERSION_ghc(9,4,1)
import GHC.Parser.Annotation(TokenLocation(NoTokenLoc))
#endif
import GHC.Parser.Annotation(noLocA)
import GHC.Parser.Annotation(reLocA)
import GHC.Parser.Annotation(noSrcSpanA)
import GHC.Parser.Annotation(noAnnSrcSpan)
--- | EP <-> ExactPrint annotation
import GHC.Parser.Annotation(EpAnn(..))

-- | Source Text

import GHC.Types.SourceText (SourceText(..))
import GHC.Types.SourceText (mkIntegralLit)
import GHC.Types.SourceText (mkTHFractionalLit)

-- | AST
import qualified GHC.Hs as GHCHsModule(HsModule(..))
import GHC.Hs(hsmodDecls)
import GHC.Hs.Decls(LHsDecl)
import GHC.Hs.Decls(HsDecl(..))
import GHC.Hs.Decls(TyClDecl(..))
import Language.Haskell.Syntax.Binds(HsBind)
import Language.Haskell.Syntax.Binds(HsLocalBinds)
import Language.Haskell.Syntax.Binds(HsBindLR(..))
import Language.Haskell.Syntax.Binds(HsLocalBindsLR(..))
import Language.Haskell.Syntax.Binds(HsValBindsLR(..))
import Language.Haskell.Syntax.Binds(HsIPBinds(..))
import Language.Haskell.Syntax.Binds(IPBind(..))
import Language.Haskell.Syntax.Expr(MatchGroup(..))
import Language.Haskell.Syntax.Expr(Match(..))
import Language.Haskell.Syntax.Expr(StmtLR(..))
import Language.Haskell.Syntax.Expr(HsTupArg(..))
import Language.Haskell.Syntax.Expr(HsExpr(..))
import Language.Haskell.Syntax.Expr(LHsExpr)
import Language.Haskell.Syntax.Lit(HsLit(..))
import Language.Haskell.Syntax.Lit(HsOverLit(..))
import Language.Haskell.Syntax.Lit(OverLitVal(..))
import Language.Haskell.Syntax.Type(HsType(..))
import Language.Haskell.Syntax.Type(HsArrow(..))
import Language.Haskell.Syntax.Type (HsTupleSort(..))
#if MIN_VERSION_ghc(9,6,1)
import Language.Haskell.Syntax.Concrete (HsUniToken(HsUnicodeTok))
#elif MIN_VERSION_ghc(9,4,1)
import Language.Haskell.Syntax.Extension (HsUniToken(HsUnicodeTok))
#else
import GHC.Parser.Annotation (IsUnicodeSyntax(..))
#endif 
import Language.Haskell.Syntax(Sig(..))
import Language.Haskell.Syntax.Type(HsWildCardBndrs(..))
import Language.Haskell.Syntax.Type(HsSigType(HsSig))
import Language.Haskell.Syntax.Pat(Pat(..))
import Language.Haskell.Syntax.Expr(GRHSs(..))
import Language.Haskell.Syntax.Expr(GRHS(..))
-- | Extensions
import Language.Haskell.Syntax.Extension (NoExtField (NoExtField))

-- | Parsing Types
import GHC(ParsedModule(..))
#if MIN_VERSION_ghc(9,4,1)
import GHC(HsToken(..))
#endif

-- | Runner Utils
import GHC(runGhc)
import GHC(setSessionDynFlags)
import GHC(getSessionDynFlags)
import GHC(setTargets)
import GHC(guessTarget)
import GHC(parseModule)
import GHC.Driver.Make(depanal)
import GHC.Driver.Make(load)
import GHC.Driver.Make(LoadHowMuch(..))
#if MIN_VERSION_ghc(9,6,1)
import GHC.Driver.Backend(noBackend)
#else
import GHC.Driver.Backend(Backend(NoBackend))
noBackend :: Backend
noBackend = NoBackend
#endif
import GHC.Driver.Session(DynFlags(..))
import GHC.Unit.Module.Graph(mgModSummaries)
import GHC(typecheckModule)
import GHC.Utils.Panic(GhcException(..))
import GHC.Driver.Monad(Ghc)
import GHC.Plugins (liftIO)
import GHC.Hs.Extension (GhcPs)
import GHC (mkModuleName)
import GHC (GhcLink(..))
import GHC.Types.SourceError (handleSourceError)
import GHC.SysTools (isContainedIn)
import GHC.Plugins (throwGhcException)
import GHC.Plugins (msHsFilePath)

-- | Data Utils
import GHC.Data.FastString(unpackFS)
import GHC.Data.FastString(mkFastString)
import GHC.Builtin.Types(integerTy)
import GHC.Data.Bag(bagToList)
import GHC.Data.Bag(listToBag)
import GHC.Data.EnumSet (insert)

-- | Names
import GHC.Types.Name.Reader(RdrName)
import GHC.Types.Name.Reader(mkRdrUnqual)
import GHC.Types.Name.Occurrence(mkTcOcc)
import GHC.Types.Name.Occurrence(mkVarOcc)
import GHC.Types.Name.Occurrence(mkDataOcc)

-- | Misc
import GHC.Plugins (PromotionFlag (NotPromoted))
import GHC.Plugins (Boxity (Boxed))
import GHC.LanguageExtensions (Extension(PartialTypeSignatures))

-- | Pretty Print
import GHC.Utils.Outputable(showPprUnsafe)
import GHC.Utils.Outputable(ppr)
import GHC.Utils.Outputable(Outputable)
import GHC.Utils.Outputable(SDoc)
#if MIN_VERSION_ghc(9,6,1)
import GHC.Base(eqString)
import GHC.Utils.Outputable(showSDocUnsafe)
eqSDoc :: SDoc -> SDoc -> Bool
eqSDoc a b = eqString (showSDocUnsafe a) (showSDocUnsafe b)
#else
eqSDoc :: SDoc -> SDoc -> Bool
eqSDoc a b = ppr a == ppr b
#endif

#if MIN_VERSION_ghc(9,6,1)
type HsModule = GHCHsModule.HsModule GhcPs
#else
type HsModule = GHCHsModule.HsModule
#endif