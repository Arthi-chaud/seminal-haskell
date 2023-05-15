# haskell-src-ext

[Hackage Page](https://hackage.haskell.org/package/haskell-src-exts-1.23.1)

Library to parse Haskell and get AST. It supports extensions (that are supported in GHC). Not-well supported, might be deprecated soon.

The library itself does not provide a way to call GHC with the AST. We would need to rely on the GHC API.

## PoCs

- `parseFile`
  - Parse file
  - Print AST
- `parsers`
  - Use Sub-parsers
