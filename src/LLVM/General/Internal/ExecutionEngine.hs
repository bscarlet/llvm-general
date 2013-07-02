{-# LANGUAGE
  MultiParamTypeClasses,
  FunctionalDependencies,
  FlexibleInstances,
  RankNTypes
  #-}
module LLVM.General.Internal.ExecutionEngine where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.AnyCont

import Data.Word
import Foreign.Ptr
import Foreign.C.String (CString)
import Foreign.C.Types (CUInt)
import Foreign.Marshal.Alloc (free)

import qualified LLVM.General.Internal.FFI.PtrHierarchy as FFI
import qualified LLVM.General.Internal.FFI.ExecutionEngine as FFI
import qualified LLVM.General.Internal.FFI.Module as FFI

import LLVM.General.Internal.Module
import LLVM.General.Internal.Context
import LLVM.General.Internal.Coding
import LLVM.General.Internal.Target
import qualified LLVM.General.AST as A

removeModule :: Ptr FFI.ExecutionEngine -> Ptr FFI.Module -> IO ()
removeModule e m = flip runAnyContT return $ do
  d0 <- alloca
  d1 <- alloca
  r <- liftIO $ FFI.removeModule e m d0 d1
  when (r /= 0) $ fail "FFI.removeModule failure"

-- | a 'ExecutableModule' e represents a 'Module' which is currently "in" an
-- 'ExecutionEngine', and so the functions of which may be executed.
data ExecutableModule e = ExecutableModule e (Ptr FFI.Module)

-- | <http://llvm.org/doxygen/classllvm_1_1ExecutionEngine.html>
class ExecutionEngine e f | e -> f where
  withModuleInEngine :: e -> Module -> (ExecutableModule e -> IO a) -> IO a
  getFunction :: ExecutableModule e -> A.Name -> IO (Maybe f)

instance ExecutionEngine (Ptr FFI.ExecutionEngine) (FunPtr ()) where
  withModuleInEngine e (Module m) = bracket_ (FFI.addModule e m) (removeModule e m) . ($ (ExecutableModule e m)) 
  getFunction (ExecutableModule e m) (A.Name name) = flip runAnyContT return $ do
    name <- encodeM name
    f <- liftIO $ FFI.getNamedFunction m name
    if f == nullPtr 
      then 
        return Nothing
      else
        do
          p <- liftIO $ FFI.getPointerToGlobal e (FFI.upCast f)
          return $ if p == nullPtr then Nothing else Just (castPtrToFunPtr p)

withExecutionEngine :: 
  Context ->
  Maybe (Ptr FFI.Module) -> 
  (Ptr (Ptr FFI.ExecutionEngine) -> Ptr FFI.Module -> Ptr CString -> IO CUInt) ->
  (Ptr FFI.ExecutionEngine -> IO a) ->
  IO a
withExecutionEngine c m createEngine f = flip runAnyContT return $ do
  liftIO initializeNativeTarget
  outExecutionEngine <- alloca
  outErrorCStringPtr <- alloca
  Module dummyModule <- maybe (anyContT $ liftM (either undefined id)
                                   . withModuleFromAST c (A.Module "" Nothing Nothing []))
                        (return . Module) m
  r <- liftIO $ createEngine outExecutionEngine dummyModule outErrorCStringPtr
  when (r /= 0) $ do
    s <- anyContT $ bracket (peek outErrorCStringPtr) free
    fail =<< decodeM s
  executionEngine <- anyContT $ bracket (peek outExecutionEngine) FFI.disposeExecutionEngine
  liftIO $ removeModule executionEngine dummyModule
  liftIO $ f executionEngine
          
-- | <http://llvm.org/doxygen/classllvm_1_1JIT.html>
newtype JIT = JIT (Ptr FFI.ExecutionEngine)

-- | bracket the creation and destruction of a 'JIT'
withJIT :: 
  Context
  -> Word -- ^ optimization level
  -> (JIT -> IO a)
  -> IO a
withJIT c opt = 
    withExecutionEngine c Nothing (\e m -> FFI.createJITCompilerForModule e m (fromIntegral opt))
    . (. JIT)

instance ExecutionEngine JIT (FunPtr ()) where
  withModuleInEngine (JIT e) m f = withModuleInEngine e m (\(ExecutableModule e m) -> f (ExecutableModule (JIT e) m))
  getFunction (ExecutableModule (JIT e) m) = getFunction (ExecutableModule e m)
