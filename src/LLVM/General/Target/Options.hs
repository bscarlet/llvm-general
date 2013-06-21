{-# LANGUAGE
  DeriveDataTypeable 
  #-}
-- | <http://llvm.org/doxygen/classllvm_1_1TargetOptions.html>
module LLVM.General.Target.Options where

import Data.Data
import Data.Word

-- | <http://llvm.org/doxygen/namespacellvm_1_1FloatABI.html#aea077c52d84934aabf9445cef9eab2e2>
data FloatABI
  = FloatABIDefault
  | FloatABISoft
  | FloatABIHard
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Typeable, Data)

-- | <http://llvm.org/doxygen/namespacellvm_1_1FPOpFusion.html#a9c71bae9f02af273833fde586d529fc5>
data FloatingPointOperationFusionMode
  = FloatingPointOperationFusionFast
  | FloatingPointOperationFusionStandard
  | FloatingPointOperationFusionStrict
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Typeable, Data)

-- | The options of a 'LLVM.General.Target.TargetOptions'
-- <http://llvm.org/doxygen/classllvm_1_1TargetOptions.html>
data Options = Options {
  printMachineCode :: Bool,
  noFramePointerElimination :: Bool,
  noFramePointerEliminationNonLeaf :: Bool,
  lessPreciseFloatingPointMultiplyAddOption :: Bool,
  unsafeFloatingPointMath :: Bool,
  noInfinitiesFloatingPointMath :: Bool,
  noNaNsFloatingPointMath :: Bool,
  honorSignDependentRoundingFloatingPointMathOption :: Bool,
  useSoftFloat :: Bool,
  noZerosInBSS :: Bool,
  jITEmitDebugInfo :: Bool,
  jITEmitDebugInfoToDisk :: Bool,
  guaranteedTailCallOptimization :: Bool,
  disableTailCalls :: Bool,
  realignStack :: Bool,
  enableFastInstructionSelection :: Bool,
  positionIndependentExecutable :: Bool,
  enableSegmentedStacks :: Bool,
  useInitArray :: Bool,
  stackAlignmentOverride :: Word32,
  trapFunctionName :: String,
  floatABIType :: FloatABI,
  allowFloatingPointOperationFusion :: FloatingPointOperationFusionMode,
  stackSmashingProtectionBufferSize :: Word32
  }
  deriving (Eq, Ord, Read, Show)

