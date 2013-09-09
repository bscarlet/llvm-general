{-# LANGUAGE
  TemplateHaskell
  #-}
module LLVM.General.Test.IR where

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck.Property

import Control.Arrow

import LLVM.General.Test.Support

import LLVM.General.Module
import LLVM.General.Arbitrary
import LLVM.General.Context
import LLVM.General.IR
import LLVM.General.PrettyPrint

tests = testGroup "IR" [
   testProperty "round-trip" $ \ast -> morallyDubiousIOProperty $ do
--     putStrLn . showPretty $ ast
     (strStd, ast') <- withContext $ \context -> withModuleFromAST' context ast $ 
       runKleisli (Kleisli moduleString &&& Kleisli moduleAST)
     let str = toIR ast'
     return $ if str == strStd
        then 
          succeeded
        else
          failed { reason = "expected: \n" ++ strStd ++ "but got: \n" ++ str ++ "\n" }
 ]
