module Changes (Change (..)) where

-- | Enum of the possible Changes to apply on the AST
-- | Inspired by the list provided in the Seminal paper (2006, p. 4)
data Change =
    -- | Replace a variable with `undefined`
    GenericReplacement |
    -- | Add arbitrary new argument to function call
    AddArg |
    -- | Delete argument to function call
    DelArg |
    -- | Swap arguments to function call
    SwapArg |
    -- | Turns an array into a tuple
    ArrayToTuple |
    -- | Turns a tuple into an array
    TupleToArray |
    -- | Turns application arguments into a tuple
    -- | Either all of them, or some
    ArgsToTuple |
    -- | Remove `case of` match
    RemoveCaseMatch |
    -- | Set value of `case of` match to undefined
    SetCaseToUndef |
    -- | Set match of `case of` match to undefined
    SetMatchToUndef |
    -- | Remove type signature
    RemoveTypeSignature |
    -- | Replace `x` in `if x then ... else ...` by `True`
    IfToTrue |
    -- | Set value of `let x = ` to `undefined`
    LetToUndef |
    -- | Set one value in `where` clause to `undefined`
    WhereToUndef