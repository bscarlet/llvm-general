-- | Tools for JIT execution
module LLVM.General.ExecutionEngine (
  ExecutionEngine(..),
  ExecutableModule,
  JIT, withJIT
  ) where

import LLVM.General.Internal.ExecutionEngine
