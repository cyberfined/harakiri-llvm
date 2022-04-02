{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE RecursiveDo      #-}

module Harakiri.Compiler.LLVM
    ( CompileParams(..)
    , translateToLLVM
    ) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State   hiding (state)
import Data.ByteString.Short        (toShort)
import Data.Char                    (ord)
import Data.HashMap.Strict          (HashMap)
import Data.IntMap                  (IntMap)
import Data.Text                    (Text)
import Data.Text.Encoding           (encodeUtf8)
import Data.Word                    (Word32)
import Harakiri.Expr                (Function(..))
import Harakiri.IR           hiding (Operand(..))
import LLVM.AST                     (Definition(..), Module(..), Name(..))
import LLVM.AST.Global              (Global(..), UnnamedAddr(..), globalVariableDefaults)
import LLVM.IRBuilder               ( MonadModuleBuilder(..), ParameterName(..)
                                    , IRBuilderT, alloca, store, load, call, add, sub, mul
                                    , sdiv, function, buildModuleT, emitDefn, fresh, br
                                    , icmp, condBr, emitBlockStart, ret, retVoid
                                    , externVarArgs
                                    )

import qualified Data.HashMap.Strict as HashMap
import qualified Data.IntMap as IntMap
import qualified Data.Text as Text
import qualified Harakiri.Expr as Harakiri
import qualified Harakiri.IR as Harakiri
import qualified LLVM.AST.AddrSpace as LLVM
import qualified LLVM.AST.IntegerPredicate as LLVM
import qualified LLVM.AST.Operand as LLVM hiding (DerivedTypeTag(..))
import qualified LLVM.AST.Type as LLVM
import qualified LLVM.AST.Constant as LLVM
import qualified LLVM.AST.Linkage as LLVM

type CompileM m = ( MonadState CompileState m
                  , MonadReader CompileContext m
                  , MonadError Text m
                  , MonadModuleBuilder m
                  , MonadFix m
                  )

data CompileContext = CompileContext
    { integerSize   :: !Word32
    , stringMap     :: !(IntMap LLVM.Operand)
    , numberFormat  :: !(Maybe LLVM.Operand)   
    , stringFormat  :: !(Maybe LLVM.Operand)
    }

data CompileState = CompileState
    { operandMap  :: !(IntMap LLVM.Operand)
    , functionMap :: !(HashMap Text LLVM.Operand)
    , labelMap    :: !(IntMap Name)
    }

data CompileParams = CompileParams
    { paramsIntegerSize :: !Word32
    }

translateToLLVM :: CompileParams -> TransResult -> Either Text Module
translateToLLVM params transRes = unError
  where unError = runExcept unModuleBuilder
        unModuleBuilder = buildModuleT "program" unReader 
        unReader = runReaderT unState initContext
        unState = runStateT translate initState
        translate = do
            printf <- externVarArgs "printf" [LLVM.ptr LLVM.i8] LLVM.void
            insertFunction "printf" printf
            scanf <- externVarArgs "scanf" [LLVM.ptr LLVM.i8] LLVM.i8
            insertFunction "scanf" scanf

            stringConsts <- flip IntMap.traverseWithKey (strings transRes) $
                \key -> genStringConst (UnName $ fromIntegral key)
            numberFmt <- getNumberFormat
            stringFmt <- genStringConst "stringFormat" "%s"
            let changeCtx ctx = ctx { stringMap    = stringConsts
                                    , numberFormat = Just numberFmt
                                    , stringFormat = Just stringFmt
                                    }
            local changeCtx $ mapM_ translateFunction (functions transRes)

        getNumberFormat :: CompileM m => m LLVM.Operand
        getNumberFormat = do
            intSize <- asks integerSize
            genStringConst "numberFormat" =<< case intSize of
                8  -> pure "%hhd"
                16 -> pure "%hd"
                32 -> pure "%d"
                64 -> pure "%lld"
                _  -> throwError "unsupported integer size"

        genStringConst :: CompileM m => Name -> Text -> m LLVM.Operand
        genStringConst strName str = do
            let charToConst = LLVM.Int 8 . fromIntegral . ord
                zeroByte = [LLVM.Int 8 0]
                charArray = LLVM.Array
                    { LLVM.memberType = LLVM.i8
                    , LLVM.memberValues = map charToConst (Text.unpack str) ++ zeroByte
                    }
                arrLength = fromIntegral (Text.length str + 1)
                arrayType = LLVM.ArrayType arrLength LLVM.i8
            emitDefn $ GlobalDefinition globalVariableDefaults
                { name        = strName
                , type'       = arrayType
                , linkage     = LLVM.External
                , isConstant  = True
                , initializer = Just charArray
                , unnamedAddr = Just GlobalAddr
                }
            let strConst =
                    LLVM.GetElementPtr True
                                       (LLVM.GlobalReference (LLVM.ptr arrayType) strName)
                                       [(LLVM.Int 32 0), (LLVM.Int 32 0)]
            pure $ LLVM.ConstantOperand strConst

        initContext :: CompileContext
        initContext = CompileContext { integerSize  = paramsIntegerSize params
                                     , stringMap    = IntMap.empty
                                     , numberFormat = Nothing
                                     , stringFormat = Nothing
                                     }

        initState :: CompileState
        initState = CompileState { operandMap  = IntMap.empty
                                 , functionMap = HashMap.empty
                                 , labelMap    = IntMap.empty
                                 }

translateFunction :: CompileM m => Function Temp [IR Temp] -> m ()
translateFunction fun = do
    retType <- irTypeToLLVM (funType fun)
    args <- mArgs
    void $ function (textToName $ funName fun) args retType $ \ops -> do
        llvmFunctionType >>= insertFunction (funName fun)
        genBody ops
  where mArgs :: CompileM m => m [(LLVM.Type, ParameterName)]
        mArgs = do
            typ <- integerType
            pure $ replicate (length $ funArgs fun) (typ, NoParameterName)

        llvmFunctionType :: CompileM m => m LLVM.Operand
        llvmFunctionType = do
            retTyp <- irTypeToLLVM (funType fun)
            argTyp <- integerType
            let argTypes = replicate (length $ funArgs fun) argTyp
            pure $ LLVM.ConstantOperand $ LLVM.GlobalReference (LLVM.PointerType
                (LLVM.FunctionType retTyp argTypes False) (LLVM.AddrSpace 0))
                    (textToName $ funName fun)

        genBody :: CompileM m => [LLVM.Operand] -> IRBuilderT m ()
        genBody args = do
            zipWithM_ setOperand (funArgs fun) args
            mapM_ translateIR (funBody fun)

        irTypeToLLVM :: CompileM m => Harakiri.Type -> m LLVM.Type
        irTypeToLLVM = \case
            Harakiri.TInt  -> integerType
            Harakiri.TVoid -> pure LLVM.void

translateIR :: CompileM m => IR Temp -> IRBuilderT m ()
translateIR = \case
    Neg dst src -> do
        res <- sub <$> intConst 0 <*> getSrcOperand src
        res >>= setOperand dst
    Binop binOp dst src1 src2 -> do
        res <- irBinopToLLVM binOp <$> getSrcOperand src1 <*> irOperandToLLVM src2
        res >>= setOperand dst
    Move dst src -> getMoveOperand src >>= setOperand dst
    Input dst -> do
        res <- getDstOperand dst
        scanf <- getFunction "scanf"
        fmt <- getNumberFormat
        void $ call scanf [(fmt, []), (res, [])]
    CallFunc dst fname args -> do
        mapM irOperandToLLVM args >>= functionCall fname >>= setOperand dst
    CallProc fname args -> void $ mapM irOperandToLLVM args >>= functionCall fname
    Echo op -> do
        out <- getEchoOperand op
        printf <- getFunction "printf"
        fmt <- case op of
            EchoString{} -> getStringFormat
            _            -> getNumberFormat
        void $ call printf [(fmt, []), (out, [])]
    Label label -> do
        lblName <- getLabelName label
        br lblName
        emitBlockStart lblName
    Branch label -> getLabelName label >>= br
    BranchIf relOp src1 src2 label -> mdo
        op1 <- getSrcOperand src1
        op2 <- irOperandToLLVM src2
        cond <- icmp (irRelopToLLVM relOp) op1 op2
        trueLbl <- getLabelName label
        condBr cond trueLbl falseLbl
        falseLbl <- fresh
        emitBlockStart falseLbl
    Return mRes -> case mRes of
        Nothing  -> retVoid
        Just res -> irOperandToLLVM res >>= ret
    _ -> pure ()
  where functionCall :: CompileM m
                     => Text
                     -> [LLVM.Operand]
                     -> IRBuilderT m LLVM.Operand
        functionCall fname args = do
            func <- getFunction fname
            call func $ fmap (,[]) args

        getFunction :: CompileM m => Text -> m LLVM.Operand
        getFunction fname =
            gets (HashMap.lookup fname . functionMap) >>= \case
                Just fn -> pure fn
                Nothing -> throwError $ "undefined function " <> fname

        getNumberFormat :: CompileM m => m LLVM.Operand
        getNumberFormat =
            asks numberFormat >>= \case
                Just fmt -> pure fmt
                Nothing  -> throwError "number format string is undefined"

        getStringFormat :: CompileM m => m LLVM.Operand
        getStringFormat =
            asks stringFormat >>= \case
                Just fmt -> pure fmt
                Nothing  -> throwError "string format string is undefined"

        irBinopToLLVM :: CompileM m
                      => Binop
                      -> LLVM.Operand
                      -> LLVM.Operand
                      -> IRBuilderT m LLVM.Operand
        irBinopToLLVM = \case
            Add -> add
            Sub -> sub
            Mul -> mul
            Div -> sdiv

        irRelopToLLVM :: Relop -> LLVM.IntegerPredicate
        irRelopToLLVM = \case
            Lt -> LLVM.SLT
            Le -> LLVM.SLE
            Gt -> LLVM.SGT
            Ge -> LLVM.SGE
            Eq -> LLVM.EQ
            Ne -> LLVM.NE

        getLabelName :: CompileM m => Label -> IRBuilderT m Name
        getLabelName (L l) = do
            gets (IntMap.lookup l . labelMap) >>= \case
                Just labelName -> pure labelName
                Nothing   -> do
                    labelName <- fresh
                    modify (insertLabel labelName)
                    pure labelName
          where insertLabel labelName state =
                    state { labelMap = IntMap.insert l labelName $ labelMap state }

        irOperandToLLVM :: CompileM m
                        => Harakiri.Operand Temp
                        -> IRBuilderT m LLVM.Operand
        irOperandToLLVM = getEchoOperand . \case
            Harakiri.Temp t  -> EchoTemp t
            Harakiri.Const c -> EchoConst c

        getMoveOperand :: CompileM m => MoveOperand Temp -> IRBuilderT m LLVM.Operand
        getMoveOperand = getEchoOperand . \case
            MoveTemp t                 -> EchoTemp t
            MoveConst c                -> EchoConst c
            MoveString (StringLabel s) -> EchoString s

        getEchoOperand :: CompileM m => EchoOperand Temp -> IRBuilderT m LLVM.Operand
        getEchoOperand = \case
            EchoTemp t  -> getSrcOperand t
            EchoConst c -> intConst c
            EchoString s ->
                asks (IntMap.lookup s . stringMap) >>= \case
                    Nothing -> throwError $ "undefined string with index "
                            <> Text.pack (show s)
                    Just operand -> pure operand

        intConst :: CompileM m => Int -> m LLVM.Operand
        intConst val = do
            bits <- asks integerSize
            pure $ LLVM.ConstantOperand $ LLVM.Int bits (fromIntegral val)

getSrcOperand :: CompileM m => Temp -> IRBuilderT m LLVM.Operand
getSrcOperand temp =
    getTemp temp >>= \case
        Just operand -> load operand 0
        Nothing      -> throwError $ "undefined temp register " <> showTemp temp

getDstOperand :: CompileM m => Temp -> IRBuilderT m LLVM.Operand
getDstOperand temp =
    getTemp temp >>= \case
        Just operand -> pure operand
        Nothing      -> do
            typ <- integerType
            operand <- alloca typ Nothing 0
            setTemp temp operand
            pure operand

setOperand :: CompileM m => Temp -> LLVM.Operand -> IRBuilderT m ()
setOperand temp operand = do
    mem <- getDstOperand temp
    store mem 0 operand

getTemp :: CompileM m => Temp -> IRBuilderT m (Maybe LLVM.Operand)
getTemp (T t) = gets (IntMap.lookup t . operandMap)

setTemp :: CompileM m => Temp -> LLVM.Operand -> IRBuilderT m ()
setTemp (T t) operand = modify $ \state ->
    state { operandMap = IntMap.insert t operand $ operandMap state }

integerType :: CompileM m => m LLVM.Type
integerType = LLVM.IntegerType <$> asks integerSize 

insertFunction :: CompileM m => Text -> LLVM.Operand -> m ()
insertFunction fname fnOp = modify $ \state ->
    state { functionMap = HashMap.insert fname fnOp $ functionMap state }

textToName :: Text -> Name
textToName = Name . toShort . encodeUtf8
