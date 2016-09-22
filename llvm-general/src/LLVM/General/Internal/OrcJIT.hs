{-# LANGUAGE MultiParamTypeClasses #-}
module LLVM.General.Internal.OrcJIT where

import LLVM.General.Prelude

import Control.Exception
import Control.Monad.AnyCont
import Control.Monad.IO.Class
import Data.Bits
import Data.ByteString (ByteString, packCString, useAsCString)
import Data.IORef
import Foreign.C.String
import Foreign.Marshal.Array (withArrayLen)
import Foreign.Ptr

import LLVM.General.Internal.Coding
import qualified LLVM.General.Internal.FFI.DataLayout as FFI
import qualified LLVM.General.Internal.FFI.LLVMCTypes as FFI
import qualified LLVM.General.Internal.FFI.OrcJIT as FFI
import qualified LLVM.General.Internal.FFI.Target as FFI
import LLVM.General.Internal.Module
import LLVM.General.Internal.Target

newtype MangledSymbol = MangledSymbol ByteString
  deriving (Show, Eq, Ord)

instance EncodeM (AnyContT IO) MangledSymbol CString where
  encodeM (MangledSymbol bs) = anyContToM $ useAsCString bs

instance MonadIO m => DecodeM m MangledSymbol CString where
  decodeM str = liftIO $ MangledSymbol <$> packCString str

data IRCompileLayer =
  IRCompileLayer {
    compileLayer :: !(Ptr FFI.IRCompileLayer),
    dataLayout :: !(Ptr FFI.DataLayout),
    cleanupActions :: !(IORef [IO ()])
  }
  deriving Eq

data JITSymbolFlags =
  JITSymbolFlags {
    jitSymbolWeak :: !Bool,
    jitSymbolExported :: !Bool
  }
  deriving (Show, Eq, Ord)

data JITSymbol =
  JITSymbol {
    jitSymbolAddress :: !WordPtr,
    jitSymbolFlags :: !JITSymbolFlags
  }
  deriving (Show, Eq, Ord)

type SymbolResolverFn = MangledSymbol -> IO JITSymbol

data SymbolResolver =
  SymbolResolver {
    dylibResolver :: !SymbolResolverFn,
    externalResolver :: !SymbolResolverFn
  }

newtype ModuleSet = ModuleSet (Ptr FFI.ModuleSetHandle)

newtype ObjectLinkingLayer = ObjectLinkingLayer (Ptr FFI.ObjectLinkingLayer)

instance Monad m => EncodeM m JITSymbolFlags FFI.JITSymbolFlags where
  encodeM f = return $ foldr1 (.|.) [
      if a f
         then b
         else 0
    | (a,b) <- [
          (jitSymbolWeak, FFI.jitSymbolFlagsWeak),
          (jitSymbolExported, FFI.jitSymbolFlagsExported)
        ]
    ]

instance Monad m => DecodeM m JITSymbolFlags FFI.JITSymbolFlags where
  decodeM f =
    return $ JITSymbolFlags {
      jitSymbolWeak = FFI.jitSymbolFlagsWeak .&. f /= 0,
      jitSymbolExported = FFI.jitSymbolFlagsExported .&. f /= 0
    }

instance MonadIO m => EncodeM m JITSymbol (Ptr FFI.JITSymbol -> IO ()) where
  encodeM (JITSymbol addr flags) = return $ \jitSymbol -> do
    flags' <- encodeM flags
    FFI.setJITSymbol jitSymbol (FFI.TargetAddress (fromIntegral addr)) flags'

instance MonadIO m => DecodeM m JITSymbol (Ptr FFI.JITSymbol) where
  decodeM jitSymbol = do
    FFI.TargetAddress addr <- liftIO $ FFI.getAddress jitSymbol
    flags <- liftIO $ decodeM =<< FFI.getFlags jitSymbol
    return (JITSymbol (fromIntegral addr) flags)

instance MonadIO m =>
  EncodeM m SymbolResolver (IORef [IO ()] -> IO (Ptr FFI.LambdaResolver)) where
  encodeM (SymbolResolver dylib external) = return $ \cleanups -> do
    dylib' <- allocFunPtr cleanups (encodeM dylib)
    external' <- allocFunPtr cleanups (encodeM external)
    FFI.createLambdaResolver dylib' external'

instance MonadIO m => EncodeM m SymbolResolverFn (FunPtr FFI.SymbolResolverFn) where
  encodeM callback =
    liftIO $ FFI.wrapSymbolResolverFn
      (\symbol result -> do
         setSymbol <- encodeM =<< callback =<< decodeM symbol
         setSymbol result)

-- | allocate a function pointer and register it for cleanup
allocFunPtr :: IORef [IO ()] -> IO (FunPtr a) -> IO (FunPtr a)
allocFunPtr cleanups alloc = mask $ \restore -> do
  funPtr <- restore alloc
  modifyIORef cleanups (freeHaskellFunPtr funPtr :)
  pure funPtr

withIRCompileLayer :: ObjectLinkingLayer -> TargetMachine -> (IRCompileLayer -> IO a) -> IO a
withIRCompileLayer (ObjectLinkingLayer oll) (TargetMachine tm) f = flip runAnyContT return $ do
  dl <- anyContToM $ bracket (FFI.createTargetDataLayout tm) FFI.disposeDataLayout
  cl <- anyContToM $ bracket (FFI.createIRCompileLayer oll tm) FFI.disposeIRCompileLayer
  cleanup <- anyContToM $ bracket (newIORef []) (sequence <=< readIORef)
  liftIO $ f (IRCompileLayer cl dl cleanup)

mangleSymbol :: IRCompileLayer -> String -> IO MangledSymbol
mangleSymbol (IRCompileLayer _ dl _) symbol = flip runAnyContT return $ do
  mangledSymbol <- alloca
  symbol' <- encodeM symbol
  anyContToM $ bracket
    (FFI.getMangledSymbol mangledSymbol symbol' dl)
    (\_ -> FFI.disposeMangledSymbol =<< peek mangledSymbol)
  decodeM =<< peek mangledSymbol

findSymbol :: IRCompileLayer -> MangledSymbol -> Bool -> IO JITSymbol
findSymbol (IRCompileLayer cl dl _) symbol exportedSymbolsOnly = flip runAnyContT return $ do
  symbol' <- encodeM symbol
  exportedSymbolsOnly' <- encodeM exportedSymbolsOnly
  symbol <- anyContToM $ bracket
    (FFI.findSymbol cl dl symbol' exportedSymbolsOnly') FFI.disposeSymbol
  decodeM symbol

findSymbolIn :: IRCompileLayer -> ModuleSet -> MangledSymbol -> Bool -> IO JITSymbol
findSymbolIn (IRCompileLayer cl dl _) (ModuleSet moduleSet) symbol exportedSymbolsOnly = flip runAnyContT return $ do
  symbol' <- encodeM symbol
  exportedSymbolsOnly' <- encodeM exportedSymbolsOnly
  symbol <- anyContToM $ bracket
    (FFI.findSymbolIn cl dl moduleSet symbol' exportedSymbolsOnly') FFI.disposeSymbol
  decodeM symbol

addModuleSet :: IRCompileLayer -> [Module] -> SymbolResolver -> IO ModuleSet
addModuleSet (IRCompileLayer cl dl cleanups) modules resolver = flip runAnyContT return $ do
  resolverAct <- encodeM resolver
  resolver' <- liftIO $ resolverAct cleanups
  modules' <- liftIO $ mapM readModule modules
  (moduleCount, modules'') <-
    anyContToM $ \f -> withArrayLen modules' $ \n hs -> f (fromIntegral n, hs)
  moduleSet <- liftIO $ FFI.addModuleSet cl dl modules'' moduleCount resolver'
  pure (ModuleSet moduleSet)

removeModuleSet :: IRCompileLayer -> ModuleSet -> IO ()
removeModuleSet (IRCompileLayer cl _ _) (ModuleSet handle) =
  FFI.removeModuleSet cl handle

withModuleSet :: IRCompileLayer -> [Module] -> SymbolResolver -> (ModuleSet -> IO a) -> IO a
withModuleSet compileLayer modules resolver =
  bracket
    (addModuleSet compileLayer modules resolver)
    (removeModuleSet compileLayer)

withObjectLinkingLayer :: (ObjectLinkingLayer -> IO a) -> IO a
withObjectLinkingLayer f =
  bracket
    FFI.createObjectLinkingLayer
    FFI.disposeObjectLinkingLayer $ \objectLayer ->
      f (ObjectLinkingLayer objectLayer)

createObjectLinkingLayer :: IO ObjectLinkingLayer
createObjectLinkingLayer = ObjectLinkingLayer <$> FFI.createObjectLinkingLayer

disposeObjectLinkingLayer :: ObjectLinkingLayer -> IO ()
disposeObjectLinkingLayer (ObjectLinkingLayer oll) =
  FFI.disposeObjectLinkingLayer oll

createIRCompileLayer :: ObjectLinkingLayer -> TargetMachine -> IO IRCompileLayer
createIRCompileLayer (ObjectLinkingLayer oll) (TargetMachine tm) = do
  dl <- FFI.createTargetDataLayout tm
  cl <- FFI.createIRCompileLayer oll tm
  cleanup <- newIORef []
  return (IRCompileLayer cl dl cleanup)

disposeIRCompileLayer :: IRCompileLayer -> IO ()
disposeIRCompileLayer (IRCompileLayer cl dl cleanup) =
  do FFI.disposeDataLayout dl
     FFI.disposeIRCompileLayer cl
     readIORef cleanup >>= sequence_
