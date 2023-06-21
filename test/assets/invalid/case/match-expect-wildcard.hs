-- This test proves that it tries to wildcard the match
c :: Int
c = case [1, 2, 3] of
    1 -> undefined
    _ -> undefined