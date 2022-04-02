{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Exception          (bracket, catch)
import Control.Monad              (when, void)
import Data.Maybe                 (fromMaybe)
import Data.Text                  (Text)
import Harakiri.Compiler.LLVM
import Harakiri.Expr       hiding (showFunction, interpret)
import Harakiri.IR         hiding (showFunction)
import Harakiri.Parser
import Harakiri.SourceCode
import Harakiri.TypeCheck
import LLVM.AST                   (Module(..))
import LLVM.Context               (Context, withContext)
import LLVM.Module                ( File(..), writeObjectToFile, withModuleFromAST
                                  , moduleLLVMAssembly
                                  )
import LLVM.PassManager           ( PassSetSpec(..), withPassManager, runPassManager
                                  , defaultCuratedPassSetSpec
                                  )
import LLVM.Target                (TargetMachine)
import System.IO                  (stderr, openFile, hClose, IOMode(..))
import System.Exit

import OptionsParser

import qualified Data.Map              as Map
import qualified Data.Set              as Set
import qualified Data.Text.IO          as TIO
import qualified Harakiri.Expr         as Expr
import qualified Harakiri.IR           as IR
import qualified LLVM.CodeModel        as CodeModel
import qualified LLVM.CodeGenOpt       as CodeGenOpt
import qualified LLVM.Exception        as LLVM
import qualified LLVM.Target           as LLVM
import qualified LLVM.Relocation       as Reloc
import qualified LLVM.AST.DataLayout   as Layout

main :: IO ()
main = do
    opts <- parseOptions
    when (showVersion opts) printVersion

    sourceCode <- SourceCode <$> TIO.readFile (inputFile opts)
    annFuncs <- rightOrPrintError (parseFromText (inputFile opts) sourceCode)
    typedFuncs <- rightOrPrintError (typeCheck sourceCode annFuncs)
    let strippedFuncs = map (fmap stripAnnotation) $ getTypedFunctions typedFuncs
    when (dumpAST opts) $
        dumpToFile (astDumpPath opts) Expr.showFunction strippedFuncs

    transRes <- rightOrPrintError (translateToIR typedFuncs)
    when (dumpIR opts) $
        dumpToFile (irDumpPath opts) IR.showFunction (IR.functions transRes)

    withTargetMachine opts $ \case
        Nothing -> die "Can't lookup target to compile"
        Just targetMachine -> do
            layout <- LLVM.getTargetMachineDataLayout targetMachine
            let integerSize = case Layout.nativeSizes layout of
                    Nothing    -> 8
                    Just sizes -> fromMaybe 8 $ Set.lookupMax sizes
                params = CompileParams integerSize
            astModule <- rightOrPrintError (translateToLLVM params transRes)
            compileToObj opts targetMachine astModule

    {-
    withContext $ \context ->
        withModuleFromAST context astModule $ \llvmModule ->
            moduleLLVMAssembly llvmModule >>= BS.putStrLn
    -}

withTargetMachine :: Options -> (Maybe TargetMachine -> IO a) -> IO a
withTargetMachine opts fn = do
    LLVM.initializeAllTargets
    catch (withFunction $ fn . Just) $ \(e :: LLVM.LookupTargetException) -> fn Nothing
  where withFunction :: (TargetMachine -> IO a) -> IO a
        withFunction fn' = case targetTriple opts of
            Nothing -> LLVM.withHostTargetMachine Reloc.PIC CodeModel.Default
                CodeGenOpt.Default fn'
            Just triple -> do
                (target, foundTriple) <- LLVM.lookupTarget Nothing triple
                LLVM.withTargetOptions $ \targetOpts -> 
                    LLVM.withTargetMachine target foundTriple "generic" Map.empty
                        targetOpts Reloc.PIC CodeModel.Default CodeGenOpt.Default fn'

printVersion :: IO ()
printVersion = do
    putStrLn "harakiri-llvm version 0.1.0.0"
    exitWith (ExitFailure 2)

rightOrPrintError :: Either Text a -> IO a
rightOrPrintError e = case e of
    Left err -> do
        TIO.hPutStrLn stderr err
        exitWith (ExitFailure 2)
    Right val -> return val

dumpToFile :: FilePath -> (a -> Text) -> [a] -> IO ()
dumpToFile fp showItem items = bracket (openFile fp WriteMode) hClose $ \hdl ->
    mapM_ (TIO.hPutStrLn hdl . showItem) items

compileToObj :: Options -> TargetMachine -> Module -> IO ()
compileToObj opts targetMachine astMod =
    withContext $ \context ->
        withModuleFromAST context astMod $ \binMod -> do
            withPassManager passes $ \passMgr -> do
                void $ runPassManager passMgr binMod
                writeObjectToFile targetMachine file binMod
  where passes = defaultCuratedPassSetSpec { optLevel = Just 3 }
        file = File (outputFile opts)
