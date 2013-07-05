{-# LANGUAGE
  TemplateHaskell,
  FlexibleInstances,
  FlexibleContexts,
  MultiParamTypeClasses,
  FunctionalDependencies,
  UndecidableInstances
  #-}
module LLVM.General.Internal.Coding where

import Language.Haskell.TH
import Language.Haskell.TH.Quote

import Control.Monad.AnyCont
import Control.Monad.IO.Class

import Data.Data (Data)
import Data.Word (Word, Word32, Word64)
import Data.Int (Int32)

import Foreign.C
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array

import qualified LLVM.General.Internal.FFI.LLVMCTypes as FFI

class EncodeM e h c where
  encodeM :: h -> e c

class DecodeM d h c where
  decodeM :: c -> d h

genCodingInstance :: Data h => TypeQ -> Name -> [(Integer, h)] -> Q [Dec]
genCodingInstance ht ctn ihs = do
  let n = const Nothing
  TyConI (NewtypeD _ _ _ (NormalC ctcn _) _) <- reify ctn
  [d| 
    instance Monad m => EncodeM m $(ht) $(conT ctn) where
      encodeM h = return $ $(
        caseE [| h |] [ match (dataToPatQ n h) (normalB (appE (conE ctcn) (litE (integerL i)))) [] | (i,h) <- ihs ] 
       )

    instance Monad m => DecodeM m $(ht) $(conT ctn) where
      decodeM i = return $ $(
        caseE [| i |] [ match (conP ctcn [litP (integerL i)]) (normalB (dataToExpQ n h)) [] | (i,h) <- ihs ]
       )
   |]

genCodingInstance' :: (Data c, Data h) => TypeQ -> Name -> [(c, h)] -> Q [Dec]
genCodingInstance' ht ctn chs = do
  let n = const Nothing
  [d| 
    instance Monad m => EncodeM m $(ht) $(conT ctn) where
      encodeM h = return $ $(
        caseE [| h |] [ match (dataToPatQ n h) (normalB (dataToExpQ n c)) [] | (c,h) <- chs ] 
       )

    instance Monad m => DecodeM m $(ht) $(conT ctn) where
      decodeM c = return $ $(
        caseE [| c |] [ match (dataToPatQ n c) (normalB (dataToExpQ n h)) [] | (c,h) <- chs ]
       )
   |]

allocaArray :: (Integral i, Storable a, MonadAnyCont IO m) => i -> m (Ptr a)
allocaArray p = anyContToM $ Foreign.Marshal.Array.allocaArray (fromIntegral p)

alloca :: (Storable a, MonadAnyCont IO m) => m (Ptr a)
alloca = anyContToM Foreign.Marshal.Alloc.alloca

peek :: (Storable a, MonadIO m) => Ptr a -> m a
peek p = liftIO $ Foreign.Storable.peek p

peekByteOff :: (Storable a, MonadIO m) => Ptr a -> Int -> m a
peekByteOff p i = liftIO $ Foreign.Storable.peekByteOff p i

poke :: (Storable a, MonadIO m) => Ptr a -> a -> m ()
poke p a = liftIO $ Foreign.Storable.poke p a

pokeByteOff :: (Storable a, MonadIO m) => Ptr a -> Int -> a -> m ()
pokeByteOff p i a = liftIO $ Foreign.Storable.pokeByteOff p i a

peekArray :: (Integral i, Storable a, MonadIO m) => i -> Ptr a -> m [a]
peekArray n p = liftIO $ Foreign.Marshal.Array.peekArray (fromIntegral n) p

instance (Monad m, EncodeM m h c, Storable c, MonadAnyCont IO m) => EncodeM m [h] (CUInt, Ptr c) where
  encodeM hs = do
    hs <- mapM encodeM hs
    (anyContToM $ \x -> Foreign.Marshal.Array.withArrayLen hs $ \n hs -> x (fromIntegral n, hs))

instance (Monad m, DecodeM m h c, Storable c, MonadIO m) => DecodeM m [h] (CUInt, Ptr c) where
  decodeM (n, ca) = do
    cs <- liftIO $ Foreign.Marshal.Array.peekArray (fromIntegral n) ca
    mapM decodeM cs

instance Monad m => EncodeM m Bool FFI.LLVMBool where
  encodeM False = return $ FFI.LLVMBool 0
  encodeM True = return $ FFI.LLVMBool 1

instance Monad m => DecodeM m Bool FFI.LLVMBool where
  decodeM (FFI.LLVMBool 0) = return $ False
  decodeM (FFI.LLVMBool 1) = return $ True

instance Monad m => EncodeM m Word CUInt where
  encodeM = return . fromIntegral

instance Monad m => EncodeM m Word32 CUInt where
  encodeM = return . fromIntegral

instance Monad m => EncodeM m Word64 CULong where
  encodeM = return . fromIntegral

instance Monad m => DecodeM m Word32 CUInt where
  decodeM = return . fromIntegral

instance Monad m => DecodeM m Word64 CULong where
  decodeM = return . fromIntegral

instance Monad m => EncodeM m Int32 CInt where
  encodeM = return . fromIntegral

instance Monad m => DecodeM m Int32 CInt where
  decodeM = return . fromIntegral

instance Monad m => DecodeM m Int CInt where
  decodeM = return . fromIntegral

instance Monad m => EncodeM m Word64 Word64 where
  encodeM = return

instance Monad m => DecodeM m Word64 Word64 where
  decodeM = return
