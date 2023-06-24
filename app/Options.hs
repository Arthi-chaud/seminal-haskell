module Options (Options(..), optionParser) where
import Options.Applicative (long, metavar, help, showDefault, value, short, eitherReader, option, auto, switch, fullDesc, header, helper, (<**>), info, ParserInfo, optional, argument, str, some)
import Seminal.Change (ChangeType(..), changeTypes)
import Text.Read (readMaybe)
import Data.List (intercalate)

data Options = Options {
    filePaths :: [String],
    -- | The number of suggestions to print.
    -- It takes the `n` best suggestions that are at least of `level` category
    n :: Maybe Int,
    -- | If true, will stop searching when a *good* change is found
    -- | If false, will go through the entire AST
    lazy :: Bool,
    -- | If True, will hide the compiler's typecheck error
    quiet :: Bool,
    -- | The minimal level of suggestions to display
    minLevel :: ChangeType,
    -- | Specify if we want to know/print the number of calls to the Typechecker
    count :: Bool
}

-- | Base parser for program options
optionParser :: ParserInfo Options
optionParser = info (parser <**> helper) description
    where
        description = fullDesc
            <> header "Seminal for Haskell"
        parser = Options <$>
            some (argument str
                ( metavar "files..."
                    <> help "The paths to the Haskell source files"
                )) <*>
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
            switch
                ( long "quiet"
                    <> short 'q'
                    <> help "Hide the original type-check error"
                ) <*>
            option changeTypeParser
                ( long "minLevel"
                    <> short 'l'
                    <> metavar "LEVEL"
                    <> showDefault
                    <> value Wrapping
                    <> help ("The minimal level of suggestions to display.\nPossible values: " ++ formattedChangeTypes)
                ) <*>
            switch
                ( long "count"
                    <> short 'c'
                    <> help "Count the number of calls to the typechecker while finding changes."
                )
        changeTypeParser = eitherReader (\i -> case readMaybe i of
            Nothing -> Left $ "Invalid Value.\nExpected one of: " ++ formattedChangeTypes
            Just e -> return e
            )
        formattedChangeTypes = intercalate ", " changeTypes