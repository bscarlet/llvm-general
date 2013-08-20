module LLVM.General.Test.Tests where

import Test.Framework

import qualified LLVM.General.Test.IR as IR

tests = testGroup "llvm-general-pure-test" [
    IR.tests
  ]
