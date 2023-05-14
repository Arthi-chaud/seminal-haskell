import Language.Haskell.Exts.Parser

parseMyType = parseType "String"

parseMyModule = do
    contents <- readFile "../assets/test.hs"
    print (parseModule contents)

parseMyImport = parseImportDecl "import Language.Haskell.Exts.Parser"

