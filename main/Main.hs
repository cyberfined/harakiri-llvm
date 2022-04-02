module Main (main) where

import Data.Text                  (Text)
import Harakiri.Compiler.LLVM
import Harakiri.Expr       hiding (showFunction, interpret)
import Harakiri.IR         hiding (showFunction)
import Harakiri.Parser
import Harakiri.SourceCode
import Harakiri.TypeCheck
import LLVM.Context               (Context, withContext)
import LLVM.Module                (withModuleFromAST, moduleLLVMAssembly)
import LLVM.PassManager           ( PassSetSpec(..), withPassManager, runPassManager
                                  , defaultCuratedPassSetSpec
                                  )
import System.IO                  (stderr)
import System.Exit

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text.IO  as TIO
import qualified Harakiri.Expr as Expr
import qualified Harakiri.IR   as IR

import Foreign.Ptr
import Control.Monad (void)
import LLVM.AST (Module(..))
import LLVM.ExecutionEngine (MCJIT, withModuleInEngine, withMCJIT, getFunction)


{-
import qualified LLVM.Relocation as Reloc
import qualified LLVM.CodeModel as CodeModel
import qualified LLVM.CodeGenOpt as CodeGenOpt
import qualified LLVM.AST.DataLayout as Layout
import LLVM.Target

withHostTargetMachine Reloc.Default CodeModel.Default CodeGenOpt.Default $ \tm -> do
    layout <- getTargetMachineDataLayout tm
    print $ Layout.nativeSizes layout
-}

foreign import ccall "dynamic" llvmFun :: FunPtr (IO ()) -> (IO ())

main :: IO ()
main = do
    sourceCode <- SourceCode <$> TIO.readFile inputFile
    annFuncs <- rightOrPrintError (parseFromText inputFile sourceCode)
    typedFuncs <- rightOrPrintError (typeCheck sourceCode annFuncs)
    let strippedFuncs = map (fmap stripAnnotation) $ getTypedFunctions typedFuncs
    mapM_ (TIO.putStrLn . Expr.showFunction) strippedFuncs

    transRes <- rightOrPrintError (translateToIR typedFuncs)
    mapM_ (TIO.putStrLn . IR.showFunction) $ IR.functions transRes
    astModule <- rightOrPrintError (translateToLLVM (CompileParams 64) transRes)

    runJIT astModule
    {-
    withContext $ \context ->
        withModuleFromAST context astModule $ \llvmModule ->
            moduleLLVMAssembly llvmModule >>= BS.putStrLn
    -}

  where inputFile = "test.hk"

rightOrPrintError :: Either Text a -> IO a
rightOrPrintError e = case e of
    Left err -> do
        TIO.hPutStrLn stderr err
        exitWith (ExitFailure 2)
    Right val -> return val

mcjit :: Context -> (MCJIT -> IO a) -> IO a
mcjit ctx = withMCJIT ctx (Just 0) Nothing Nothing Nothing

runJitFn :: FunPtr a -> IO ()
runJitFn fn = llvmFun (castFunPtr fn :: FunPtr (IO ()))

runJIT :: Module -> IO ()
runJIT mod =
    withContext $ \context ->
        withModuleFromAST context mod $ \m -> do
            withPassManager passes $ \passMgr -> do
                void $ runPassManager passMgr m
                s <- moduleLLVMAssembly m
                BS.putStrLn s
                mcjit context $ \engine ->
                    withModuleInEngine engine m $ \em ->
                        getFunction em "main" >>= \case
                            Nothing     -> pure ()
                            Just mainFn -> void $ runJitFn mainFn
  where passes = defaultCuratedPassSetSpec { optLevel = Just 3 }
