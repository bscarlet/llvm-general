import Test.Framework
import qualified LLVM.General.Test.Tests as LLVM.General
import qualified Numeric.Test.Tests as Numeric

main = defaultMain [
        LLVM.General.tests,
        Numeric.tests
   ]
