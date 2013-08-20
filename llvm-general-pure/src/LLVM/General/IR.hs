module LLVM.General.IR (
 IR(..)
 ) where

import Text.ParserCombinators.Parsec
import qualified LLVM.General.AST as A

class IR a where
  toIR :: a -> String
  fromIR :: Parser a

instance IR A.Module where
  toIR m = ""
  fromIR = return A.defaultModule
