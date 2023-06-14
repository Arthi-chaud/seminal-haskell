-- | Provide a way to access Compiler/Typechecker as a black box,
-- | according to Seminal's algorithm
module Seminal.Compiler.TypeChecker (
    TypeCheckStatus (..),
    ErrorType (..), isScopeError, isTypecheckError, getTypeCheckError,
    Seminal.Compiler.TypeChecker.typecheckModule
) where

import GHC (typecheckModule, ParsedModule, Ghc)
import Data.Monoid ()
import GHC.Types.SourceError (handleSourceError)
import Text.Printf (printf)
import GHC.SysTools (isContainedIn)
import Data.Functor ((<&>))
import Data.Text (pack, strip, unpack)

-- | Defines the possible outcomes of the typechecking process of the compiler
data TypeCheckStatus =
    -- | Indicated the Code typechecks
    Success |
    -- | An error happened while typechecking
    Error ErrorType
    deriving (Show, Eq)

data ErrorType =
    -- | Indicates an error occured while typechecking
    TypeCheckError String |
    -- | A type or variable could not be resolved
    -- | It comes with the compiler's error message
    ScopeError String

getTypeCheckError :: TypeCheckStatus -> Maybe ErrorType
getTypeCheckError (Error e) = return e
getTypeCheckError _ = Nothing

isScopeError :: ErrorType -> Bool
isScopeError (ScopeError _) = True
isScopeError _ = False

isTypecheckError :: ErrorType -> Bool
isTypecheckError (TypeCheckError _) = True
isTypecheckError _ = False 

instance Eq ErrorType where
    TypeCheckError _ == TypeCheckError _ = True
    ScopeError _ == ScopeError _ = True
    _ == _ = False

-- | Pretty-print of Error types
instance Show ErrorType where
    show (TypeCheckError err) = printf "Typecheck Error:\n%s" err
    show (ScopeError err) = printf "Scope Error:\n%s" err
    
-- | Typecheck Module
typecheckModule :: ParsedModule -> Ghc TypeCheckStatus
typecheckModule parsedModule = do
    maybeT <- handleSourceError (return . Left . show) (GHC.typecheckModule parsedModule <&> Right)
    return $ case maybeT of
        Right _ -> Success
        Left errMsg -> Error (if any (`isContainedIn` errMsg) ["Variable not in scope", "Not in scope: type constructor or class"]
            then ScopeError strippedMsg
            else TypeCheckError strippedMsg)
                where strippedMsg = unpack (strip $ pack errMsg)

