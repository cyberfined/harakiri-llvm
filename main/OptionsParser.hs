module OptionsParser
    ( Options(..)
    , parseOptions
    , astDumpPath
    , irDumpPath
    , llvmDumpPath
    ) where

import Control.Monad         (when)
import Data.ByteString.Short (ShortByteString)
import Options.Applicative
import System.Exit
import System.FilePath

data Options = Options
    { showVersion  :: !Bool
    , dumpAST      :: !Bool
    , dumpIR       :: !Bool
    , dumpLLVM     :: !Bool
    , targetTriple :: !(Maybe ShortByteString)
    , inputFile    :: !FilePath
    , outputFile   :: !FilePath
    }

parseOptions :: IO Options
parseOptions = do
    opts <- execParser (info optionsParser fullDesc)
    when (not (showVersion opts) && null (inputFile opts)) $ do
        putStrLn "error: no input file"
        exitWith (ExitFailure 2)
    when (not (null $ inputFile opts) && takeExtension (inputFile opts) /= ".hk") $ do
        putStrLn "error: input file's extension must be .hk"
        exitWith (ExitFailure 2)
    return opts

optionsParser :: Parser Options
optionsParser = options <**> helper

options :: Parser Options
options =  Options
       <$> switch
           ( long "version"
          <> short 'v'
          <> help "Display compiler version information"
           )
       <*> switch
           ( long "dump-ast"
          <> help "Dump AST representation"
           )
       <*> switch
           ( long "dump-ir"
          <> help "Dump intermediate representation"
           )
       <*> switch
           ( long "dump-llvm"
          <> help "Dump llvm assembly"
           )
       <*> (optional . strOption)
           ( long "target"
          <> short 't'
          <> metavar "TARGET"
          <> help "Set target"
           )
       <*> strOption
           ( short 'c'
          <> metavar "PATH"
          <> value ""
          <> noArgError (ErrorMsg "No input file")
          <> help "Set source file"
           )
       <*> strOption
           ( short 'o'
          <> metavar "PATH"
          <> value ""
          <> help "Set output file"
           )

astDumpPath :: Options -> FilePath
astDumpPath opts = dropExtension (inputFile opts) ++ "-ast"

irDumpPath :: Options -> FilePath
irDumpPath opts = dropExtension (inputFile opts) ++ "-ir"

llvmDumpPath :: Options -> FilePath
llvmDumpPath opts = replaceExtension (inputFile opts) "ll"
