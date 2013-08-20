import Test.QuickCheck

import qualified LLVM.General.AST as A

import LLVM.General.Arbitrary
import LLVM.General.PrettyPrint

main = do
  s <- sample' (arbitrary :: Gen A.Module)
  mapM_ (putStrLn . showPretty) s
