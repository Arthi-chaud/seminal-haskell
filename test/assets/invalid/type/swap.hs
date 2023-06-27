type SrcSpan = String

type Change a = a

type Enumerator a = a -> SrcSpan -> [Change a]

enumMe :: Enumerator Int
enumMe "AB" 1 = [1]