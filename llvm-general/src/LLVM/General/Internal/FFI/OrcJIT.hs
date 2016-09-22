{-# LANGUAGE ForeignFunctionInterface #-}

module LLVM.General.Internal.FFI.OrcJIT where

import LLVM.General.Prelude

import Foreign.C
import Foreign.Ptr

import LLVM.General.Internal.FFI.DataLayout
import LLVM.General.Internal.FFI.LLVMCTypes
import LLVM.General.Internal.FFI.Module
import LLVM.General.Internal.FFI.Target

data IRCompileLayer
data JITSymbol
data LambdaResolver
data ModuleSetHandle
data ObjectLinkingLayer

newtype TargetAddress = TargetAddress Word64

type SymbolResolverFn = CString -> Ptr JITSymbol -> IO ()

foreign import ccall "wrapper" wrapSymbolResolverFn ::
  SymbolResolverFn -> IO (FunPtr SymbolResolverFn)

foreign import ccall safe "LLVM_General_createIRCompileLayer" createIRCompileLayer ::
  Ptr ObjectLinkingLayer -> Ptr TargetMachine -> IO (Ptr IRCompileLayer)

foreign import ccall safe "LLVM_General_disposeIRCompileLayer" disposeIRCompileLayer ::
  Ptr IRCompileLayer -> IO ()

foreign import ccall safe "LLVM_General_IRCompileLayer_findSymbol" findSymbol ::
  Ptr IRCompileLayer -> Ptr DataLayout -> CString -> LLVMBool -> IO (Ptr JITSymbol)

foreign import ccall safe "LLVM_General_IRCompileLayer_findSymbolIn" findSymbolIn ::
  Ptr IRCompileLayer -> Ptr DataLayout -> Ptr ModuleSetHandle -> CString -> LLVMBool -> IO (Ptr JITSymbol)

foreign import ccall safe "LLVM_General_disposeJITSymbol" disposeSymbol ::
  Ptr JITSymbol -> IO ()

foreign import ccall safe "LLVM_General_createLambdaResolver" createLambdaResolver ::
  FunPtr SymbolResolverFn ->
  FunPtr SymbolResolverFn ->
  IO (Ptr LambdaResolver)

foreign import ccall safe "LLVM_General_IRCompileLayer_addModuleSet" addModuleSet ::
  Ptr IRCompileLayer ->
  Ptr DataLayout ->
  Ptr (Ptr Module) ->
  CUInt ->
  Ptr LambdaResolver ->
  IO (Ptr ModuleSetHandle)

foreign import ccall safe "LLVM_General_IRCompileLayer_removeModuleSet" removeModuleSet ::
  Ptr IRCompileLayer -> Ptr ModuleSetHandle -> IO ()

foreign import ccall safe "LLVM_General_createObjectLinkingLayer" createObjectLinkingLayer ::
  IO (Ptr ObjectLinkingLayer)

foreign import ccall safe "LLVM_General_disposeObjectLinkingLayer" disposeObjectLinkingLayer ::
  Ptr ObjectLinkingLayer -> IO ()

foreign import ccall safe "LLVM_General_JITSymbol_getAddress" getAddress ::
  Ptr JITSymbol -> IO TargetAddress

foreign import ccall safe "LLVM_General_JITSymbol_getFlags" getFlags ::
  Ptr JITSymbol -> IO JITSymbolFlags

foreign import ccall safe "LLVM_General_setJITSymbol" setJITSymbol ::
  Ptr JITSymbol -> TargetAddress -> JITSymbolFlags -> IO ()

foreign import ccall safe "LLVM_General_getMangledSymbol" getMangledSymbol ::
  Ptr CString -> CString -> Ptr DataLayout -> IO ()

foreign import ccall safe "LLVM_General_disposeMangledSymbol" disposeMangledSymbol ::
  CString -> IO ()
