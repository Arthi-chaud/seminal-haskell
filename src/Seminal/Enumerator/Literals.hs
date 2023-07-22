module Seminal.Enumerator.Literals (enumerateChangeInLiteral, enumerateRaiseInLiterals) where
import Seminal.Enumerator.Enumerator (Enumerator)
import Seminal.Compiler.API
import Seminal.Change (Change(..), ChangeType (Terminal), node)
import Data.ByteString (unpack)
import Data.ByteString.Internal (w2c)
import Data.Char (isDigit, digitToInt)
import Text.Read (readMaybe)
import Data.Maybe (mapMaybe)
import Data.Functor ((<&>))
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
        changeForChar char = (
            Change (node literal) [node $ HsString NoSourceText (mkFastString [char])] loc []
            "The expected type of the expression is a String, not a Char. Use double quotes or brackets instead." Terminal -- Turn into String
            ): [(Change (node literal) [node (charToInt char)] loc [] "Expected an Int, not a Char. Remove the quotes" Terminal) | isDigit char] -- Turn into Int
        changeForString [char] = (
            Change (node literal) [node $ HsChar NoSourceText char] loc []
            "The expected type of the expression is a Char, not a String. Use single quotes instead." Terminal -- If Singleton, extract it to char
            ): [(Change (node literal) [node (charToInt char)] loc [] "Expected an Int, not a String. Remove the quotes" Terminal) | isDigit char]
        changeForString str = mapMaybe (\x -> x str) [stringToInteger, stringToInt, stringToFloat, stringToDouble]
            <&> (\newNum -> Change (node literal) [node newNum] loc [] "Remove the quotes." Terminal)
        charToInt = HsInt NoExtField . mkIntegralLit . digitToInt
        stringToInteger :: String -> Maybe (HsLit GhcPs)
        stringToInteger str = (\x -> HsInteger NoSourceText x integerTy) <$> (readMaybe str :: Maybe Integer)
        stringToInt :: String -> Maybe (HsLit GhcPs)
        stringToInt str = (HsInt NoExtField . mkIntegralLit) <$> (readMaybe str :: Maybe Int)
        stringToDouble :: String -> Maybe (HsLit GhcPs)
        stringToDouble str = (HsDoublePrim NoExtField . mkTHFractionalLit . toRational) <$> (readMaybe str :: Maybe Double)
        stringToFloat :: String -> Maybe (HsLit GhcPs)
        stringToFloat str = (HsFloatPrim NoExtField . mkTHFractionalLit . toRational) <$> (readMaybe str :: Maybe Float)

-- | Enumerate changes that could be done, involving turning a literal into an overloaded literal
enumerateRaiseInLiterals :: Enumerator (HsExpr GhcPs)
enumerateRaiseInLiterals expr loc  = case expr of
    (HsLit x lit) -> case lit of
        (HsString _ string) -> changeForString $ unpackFS string
        (HsStringPrim _ string) -> changeForString $ w2c <$> unpack string
        _ -> []
        where
            changeForString s = overLitValToLit <$> mapMaybe (\f -> f s) [stringToDouble]
                <&> (\newLit -> Change (node expr) [node $ HsOverLit x newLit] loc [] "Remove the quotes" Terminal)
            stringToDouble str = nbToOverLitVal <$> (readMaybe str :: Maybe Double)
            overLitValToLit = OverLit NoExtField
            nbToOverLitVal = HsFractional . mkTHFractionalLit . toRational
    _ -> []