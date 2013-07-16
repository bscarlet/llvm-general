-- | <http://llvm.org/docs/LangRef.html#data-layout>
-- Most functions in this module return 'Maybe'. This is because not all 'Type's are representable in memory (consider 'VoidType' and 'MetadataType'), and 'DataLayout's do not contain information for all types representable in LLVM IR. For example, most 'DataLayout's do not describe pointer types having address spaces other than zero, or floating point types unsupported by the target.
-- 'NamedTypeReference's cannot be resolved by these functions and will generally produce 'Nothing'.
module LLVM.General.DataLayout
    ( typeSizeInBits,
      typeAllocSize, typeAllocSizeInBits,
      typeStoreSize, typeStoreSizeInBits,
      typeABIAlignment, typePreferredAlignment,
      pointerSize, intPtrType
    ) where

import Data.Word
import Data.Bits
import Control.Applicative
import Control.Monad

import Data.Map (Map)
import qualified Data.Map as Map

import LLVM.General.AST.DataLayout
import LLVM.General.AST.Type
import LLVM.General.AST.AddrSpace

-- | The number of bits necessary to represent in memory a particular 'Type' under a particular 'DataLayout'.
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

-- | A description of the in-memory layout of values of a specific StructureType
data StructureLayout = StructureLayout {
    structureSize :: Word,
    structureAlignment :: Word,
    memberOffsets :: [Word]
  }

-- | The layout descriptor for an optionally packed structure type containing certain elements.
structLayout :: DataLayout
             -> Bool -- ^ Whether the structure type is packed
             -> [Type] -- ^ The elements of the structure type
             -> Maybe StructureLayout
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

-- | Round up a value representing a size or position in memory to a particular alignment.
-- | For example, 7 rounded up to an alignment of 4 is 8.  8 rounded up to the alignment of 4 is 8 because it is already aligned.
roundUpAlignment :: Word -- ^ Value to be rounded
                 -> Word -- ^ Alignment
                 -> Word
roundUpAlignment value alignment = (value + (alignment - 1)) .&. complement (alignment - 1)

-- | The byte offset between consecutive aligned values of this type, equal to space reserved by alloca
typeAllocSize :: DataLayout -> Type -> Maybe Word
typeAllocSize l ty = roundUpAlignment <$> (typeStoreSize l ty) <*> (typeAlignment True l ty)

-- | As 'typeAllocSize', but in bits
typeAllocSizeInBits :: DataLayout -> Type -> Maybe Word
typeAllocSizeInBits l ty = (8 *) <$> typeAllocSize l ty

-- | The maximum number of bytes that may be overwritten by storing a value of this type
typeStoreSize :: DataLayout -> Type -> Maybe Word
typeStoreSize l ty = ((`div` 8) . (+ 7)) <$> typeSizeInBits l ty

-- | As 'typeStoreSize', but in bits
typeStoreSizeInBits :: DataLayout -> Type -> Maybe Word
typeStoreSizeInBits l ty = (8 *) <$> typeStoreSize l ty

-- | The boundary on which memory representing a value of a particular 'Type' should be aligned
typeAlignment :: Bool -- ^ True to obtain the ABI (minimum) alignment, false for the preferred (optimal) alignment
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

-- | The minimum boundary on which memory representing a value of a particular 'Type' must be aligned
typeABIAlignment :: DataLayout -> Type -> Maybe Word
typeABIAlignment = typeAlignment True

-- | The boundary on which memory representing a value of a particular 'Type' should be aligned for most efficient access
typePreferredAlignment :: DataLayout -> Type -> Maybe Word
typePreferredAlignment = typeAlignment False

-- | Lookup helper for typeAlignment
alignmentInfo :: AlignType -> Bool -> DataLayout -> Word32 -> Maybe Word
alignmentInfo alignTy isABI l bits = do
  AlignmentInfo { abiAlignment = abi, preferredAlignment = pref } <-
      Map.lookup (alignTy, bits) . typeLayouts $ l
  fromIntegral <$> if isABI then return abi else pref `mplus` return abi

-- | Convenience function for 'typeSizeInBits' on a 'PointerType' of a particular address space
pointerSize :: DataLayout -> AddrSpace -> Maybe Word
pointerSize layout addrSpace = typeSizeInBits layout (PointerType (IntegerType 8) addrSpace)

-- | An integer type of the same size as a pointer in the given address space
intPtrType :: DataLayout -> AddrSpace -> Maybe Type
intPtrType layout addrSpace = IntegerType . fromIntegral <$> pointerSize layout addrSpace
