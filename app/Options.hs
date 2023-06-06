{-# LANGUAGE LambdaCase #-}
module Options (Options(..), optionParser) where
import Options.Applicative (long, metavar, help, showDefault, value, short, eitherReader, option, auto, switch, fullDesc, header, helper, (<**>), info, ParserInfo, optional, argument, str)
import Seminal (ChangeType(..))

data Options = Options {
    filePath :: String,
    -- | The number of suggestions to print.
    -- It takes the `n` best suggestions that are at least of `level` category
    n :: Maybe Int,
    -- | If true, will stop searching when a *good* change is found
    -- | If false, will go through the entire AST
    lazy :: Bool,
    -- | The minimal level of suggestions to display
    minLevel :: ChangeType
}

-- | Base parser for program options
optionParser :: ParserInfo Options
optionParser = info (parser <**> helper) description
    where
        description = fullDesc
            <> header "Seminal for Haskell"
        parser = Options <$>
            argument str
                ( metavar "filePath"
                    <> help "The path to the Haskell source file"
                ) <*>
            optional (option auto
                ( long "lines"
                    <> short 'n'
                    <> metavar "N"
                    <> help "Output the best N suggestions"
                )) <*>
            switch
                ( long "lazy"
                    <> help "Stops searching at the first *good* change"
                ) <*>
            option changeTypeParser
                ( long "minLevel"
                    <> short 'l'
                    <> metavar "LEVEL"
                    <> showDefault
                    <> value Wrapping
                    <> help "The minimal level of suggestions to display"
                )
        changeTypeParser = eitherReader $ \case
            "Terminal" -> Right Terminal
            "Wildcard" -> Right Wildcard
            "Removal" -> Right Removal
            "Wrapping" -> Right Wrapping
            _ -> Left "Invalid Value"