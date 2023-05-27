-- Provide a way to access Compiler/Typechecker as a black box,
-- according to Seminal's algorithm
module Compiler.TypeChecker (
    TypeCheckStatus (..),
    ErrorType (..),
    Compiler.TypeChecker.typecheckModule
) where

import GHC
    ( typecheckModule, ParsedModule, Ghc )
import Data.Monoid ()
import GHC.Types.SourceError (handleSourceError)
import Text.Printf (printf)
import GHC.SysTools (isContainedIn)
import Data.Functor ((<&>))

-- Defines the possible outcomes of the typechecking process of the compiler
data TypeCheckStatus =
    -- Indicated the Code typechecks
    Success |
    -- An error happened while typechecking
    Error ErrorType
    deriving Show

data ErrorType =
    -- Indicates an error occured while typechecking
    TypeCheckError |
    -- A type or variable could not be resolved
    -- It comes with the compiler's error message
    ScopeError String

-- Pretty-print of Error types
instance Show ErrorType where
    show TypeCheckError = "Type Checking Error"
    show (ScopeError err) = printf "Scope Error: %s" err


-- Typecheck Module
typecheckModule :: ParsedModule -> Ghc TypeCheckStatus
typecheckModule parsedModule = do
    maybeT <- handleSourceError (return . Left . show) (GHC.typecheckModule parsedModule <&> Right)
    return $ case maybeT of
        Right _ -> Success
        Left errMsg -> Error (if any (`isContainedIn` errMsg) ["Variable not in scope", "Not in scope: type constructor or class"]
            then ScopeError errMsg
            else TypeCheckError)

