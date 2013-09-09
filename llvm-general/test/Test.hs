import Test.Framework
import Test.Framework.Seed
import System.Random
import qualified LLVM.General.Test.Tests as LLVM.General
import LLVM.General.CommandLine

main = do
  (stdGen, i) <- if True then newSeededStdGen (FixedSeed (-3527836534093460058)) else newStdGenWithKnownSeed 
  putStrLn $ "using initial stdGen seed: " ++ show i
  setStdGen stdGen
  parseCommandLineOptions [
    "test",
    "-bb-vectorize-ignore-target-info"
   ] Nothing
  defaultMain [
        LLVM.General.tests
   ]
