module Enumerator.Literals (enumerateChangeInLiteral) where
import Enumerator.Enumerator (Enumerator)
import GHC (HsLit(..), GhcPs)
import Change (newChange, ChangeType (Terminal))
import Data.ByteString (unpack)
import Data.ByteString.Internal (w2c)
import GHC.Data.FastString (mkFastString)
import GHC.Types.SourceText (SourceText(NoSourceText))
import GHC.Plugins (unpackFS)

-- | Enumeration of changes for Literals, e.g. hard-coded chars, strings, ints etc.
-- See [API doc](https://hackage.haskell.org/package/ghc-9.6.1/docs/Language-Haskell-Syntax-Lit.html#t:HsLit)
enumerateChangeInLiteral :: Enumerator (HsLit GhcPs)
enumerateChangeInLiteral literal loc = case literal of
    (HsChar _ char) -> changeForChar char
    (HsCharPrim _ char) -> changeForChar char
    (HsString _ string) ->
        -- Need to turn FastString to String, for homogeneity
        changeForString $ unpackFS string
    (HsStringPrim _ string) ->
        -- Need to turn ByteString to String, for homogeneity
        changeForString $ w2c <$> unpack string
    _ -> []
    where
        changeForChar char = [
            newChange literal (HsString NoSourceText (mkFastString [char])) loc [] Nothing Terminal -- Turn into String
            ]
        changeForString [char] = [
            newChange literal (HsChar NoSourceText char) loc [] Nothing Terminal -- If Singleton, extract it to char
            ]
        changeForString _ = []