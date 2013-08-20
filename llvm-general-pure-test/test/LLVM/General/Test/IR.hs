module LLVM.General.Test.IR where

import Test.Framework
import Test.Framework.Providers.QuickCheck2

import Data.Either

import LLVM.General.Arbitrary
import LLVM.General.IR
import qualified LLVM.General.AST as A

import Text.ParserCombinators.Parsec (parse)

tests = testGroup "IR" [
{-
   testProperty "pure round-trip" $
     ((\ast -> either (const False) (== ast) . parse fromIR "" . toIR $ ast) :: A.Module -> Bool)
-}
 ]
