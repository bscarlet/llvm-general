-- | <http://llvm.org/docs/LangRef.html#data-layout>
module LLVM.General.AST.DataLayout where

import Data.Word
import Data.Bits
import Control.Applicative
import Control.Monad

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)

import LLVM.General.AST.AddrSpace
import LLVM.General.AST.Type

-- | Little Endian is the one true way :-). Sadly, we must support the infidels.
data Endianness = LittleEndian | BigEndian
  deriving (Eq, Ord, Read, Show)

-- | An AlignmentInfo describes how a given type must and would best be aligned
data AlignmentInfo = AlignmentInfo {
    abiAlignment :: Word32,
    preferredAlignment :: Maybe Word32
  }
  deriving (Eq, Ord, Read, Show)

-- | A type of type for which 'AlignmentInfo' may be specified
data AlignType
  = IntegerAlign
  | VectorAlign
  | FloatAlign
  | AggregateAlign
  | StackAlign
  deriving (Eq, Ord, Read, Show)

-- | a description of the various data layout properties which may be used during
-- optimization
data DataLayout = DataLayout {
    endianness :: Maybe Endianness,
    stackAlignment :: Maybe Word32,
    pointerLayouts :: Map AddrSpace (Word32, AlignmentInfo),
    typeLayouts :: Map (AlignType, Word32) AlignmentInfo,
    nativeSizes :: Maybe (Set Word32)
  }
  deriving (Eq, Ord, Read, Show)

-- | A description of the in-memory layout of values of a specific StructureType
data StructureLayout = StructureLayout {
    structureSize :: Word,
    structureAlignment :: Word,
    memberOffsets :: [Word]
  }

-- | a 'DataLayout' which specifies nothing
defaultDataLayout = DataLayout {
  endianness = Nothing,
  stackAlignment = Nothing,
  pointerLayouts = Map.empty,
  typeLayouts = Map.empty,
  nativeSizes = Nothing
 }

typeSizeInBits :: DataLayout -> Type -> Maybe Word
typeSizeInBits _ (IntegerType { typeBits = bits }) = Just (fromIntegral bits)
typeSizeInBits l (PointerType { pointerAddrSpace = addrSpace }) =
    fmap (fromIntegral . fst) . Map.lookup addrSpace . pointerLayouts $ l
typeSizeInBits _ (FloatingPointType { typeBits = bits }) = Just (fromIntegral bits)
typeSizeInBits l (VectorType { nVectorElements = n, elementType = ty }) =
    (* fromIntegral n) <$> typeSizeInBits l ty
typeSizeInBits l (StructureType { isPacked = packed, elementTypes = tys }) =
    structureSize <$> structLayout l packed tys
typeSizeInBits l (ArrayType { nArrayElements = n, elementType = ty }) =
    (fromIntegral n *) <$> typeAllocSizeInBits l ty
typeSizeInBits _ VoidType = Nothing
typeSizeInBits _ (FunctionType {}) = Nothing
typeSizeInBits _ (NamedTypeReference _) = Nothing
typeSizeInBits _ MetadataType = Nothing

structLayout :: DataLayout -> Bool -> [Type] -> Maybe StructureLayout
structLayout l packed tys =
    do (align', size', offsets') <- foldM step (0,0,[]) tys
       let align = min 1 align'
           size = if size' .&. (align - 1) /= 0
                  then roundUpAlignment size' align
                  else size'
           offsets = reverse offsets'
       return $ StructureLayout { structureAlignment = align, structureSize = size, memberOffsets = offsets }
  where
    step :: (Word, Word, [Word]) -> Type -> Maybe (Word, Word, [Word])
    step (align, size, offsets) ty = do
        tyAlign <- if packed then Just 1 else typeAlignment True l ty
        let offset = if size .&. (tyAlign - 1) /= 0
                     then roundUpAlignment size tyAlign
                     else size
        tyAllocSize <- typeAllocSize l ty
        return (max tyAlign align, offset + tyAllocSize, offset:offsets)

roundUpAlignment :: Word -> Word -> Word
roundUpAlignment value alignment = (value + (alignment - 1)) .&. complement (alignment - 1)

-- | The offset between consecutive aligned values of this type, equal to space reserved by alloca
typeAllocSize :: DataLayout -> Type -> Maybe Word
typeAllocSize l ty = roundUpAlignment <$> (typeStoreSize l ty) <*> (typeAlignment True l ty)

typeAllocSizeInBits :: DataLayout -> Type -> Maybe Word
typeAllocSizeInBits l ty = (8 *) <$> typeAllocSize l ty

-- | The maximum number of bytes that may be overwritten by storing a value of this type
typeStoreSize :: DataLayout -> Type -> Maybe Word
typeStoreSize l ty = ((`div` 8) . (+ 7)) <$> typeSizeInBits l ty

typeStoreSizeInBits :: DataLayout -> Type -> Maybe Word
typeStoreSizeInBits l ty = (8 *) <$> typeStoreSize l ty

typeAlignment :: Bool -- ^ True for the ABI (minimum) alignment, false for the preferred alignment
              -> DataLayout -> Type -> Maybe Word
typeAlignment isABI l (IntegerType { typeBits = bits }) = alignmentInfo IntegerAlign isABI l bits
typeAlignment isABI l (PointerType { pointerAddrSpace = addrSpace }) = do
  (_, AlignmentInfo { abiAlignment = abi, preferredAlignment = pref }) <-
      Map.lookup addrSpace . pointerLayouts $ l
  fromIntegral <$> if isABI then return abi else pref `mplus` return abi
typeAlignment isABI l (FloatingPointType { typeBits = bits }) = alignmentInfo FloatAlign isABI l bits
typeAlignment isABI l ty@(VectorType {}) =
    alignmentInfo VectorAlign isABI l =<< fromIntegral <$> typeSizeInBits l ty
typeAlignment isABI l (StructureType { isPacked = packed, elementTypes = tys }) =
    if packed && isABI then Just 1
    else max <$> (structureAlignment <$> structLayout l packed tys) <*> alignmentInfo AggregateAlign isABI l 0
typeAlignment isABI l (ArrayType { elementType = ty }) = typeAlignment isABI l ty
typeAlignment _ _ (FunctionType {}) = Nothing
typeAlignment _ _ VoidType = Nothing
typeAlignment _ _ (NamedTypeReference _) = Nothing
typeAlignment _ _ MetadataType = Nothing

alignmentInfo :: AlignType -> Bool -> DataLayout -> Word32 -> Maybe Word
alignmentInfo alignTy isABI l bits = do
  AlignmentInfo { abiAlignment = abi, preferredAlignment = pref } <-
      Map.lookup (alignTy, bits) . typeLayouts $ l
  fromIntegral <$> if isABI then return abi else pref `mplus` return abi
