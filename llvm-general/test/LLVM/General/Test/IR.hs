{-# LANGUAGE
  TemplateHaskell
  #-}
module LLVM.General.Test.IR where

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck.Property

import LLVM.General.Test.Support

import LLVM.General.Module
import LLVM.General.Context
import LLVM.General.IR

tests = testGroup "IR" [
   testProperty "round-trip" $ \ast -> morallyDubiousIOProperty $ do
     std <- withContext $ \context -> withModuleFromAST' context ast moduleString
     let actual = toIR ast
     return $ if actual == std
        then 
          succeeded
        else
          failed { reason = "expected: \n" ++ std ++ "but got: " ++ actual }
 ]
