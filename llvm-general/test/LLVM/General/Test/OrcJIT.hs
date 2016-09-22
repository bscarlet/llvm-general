{-# LANGUAGE ForeignFunctionInterface #-}
module LLVM.General.Test.OrcJIT where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import LLVM.General.Test.Support

import Data.Foldable
import Data.IORef
import Data.Word
import Foreign.Ptr
import qualified System.Info as Info

import LLVM.General.Context
import LLVM.General.Module
import LLVM.General.OrcJIT
import LLVM.General.Target

-- | A main-style function that calls back into Haskell
testModule1 :: String
testModule1 =
  "; ModuleID = '<string>'\n\
  \source_filename = \"<string>\"\n\
  \\n\
  \declare i32 @testFunc()\n\
  \define i32 @main(i32, i8*) {\n\
  \  %3 = call i32 @testFunc()\n\
  \  ret i32 %3\n\
  \}\n"

-- | A trivial main-style function.
testModule3 :: String
testModule3 =
  unlines [ "; ModuleID = '<string>'"
          , "source_filename = \"<string>\""
          , ""
          , "define i32 @main(i32, i8*) {"
          , "ret i32 42"
          , "}"]

-- | Function that takes and uses numeric arguments
testModule2 :: String
testModule2 =
  unlines [ "; ModuleID = 'test.c'"
          , "define double @foo(double %x, double %y) #0 {"
          , "  %1 = fadd double %x, %y"
          , "  ret double %1"
          , "}" ]

withAssembly :: String -> (Module -> IO a) -> IO a
withAssembly src f = withContext $ \context ->
                       withModuleFromLLVMAssembly' context src f

myTestFuncImpl :: IO Word32
myTestFuncImpl = return 42

foreign import ccall "wrapper"
  wrapTestFunc :: IO Word32 -> IO (FunPtr (IO Word32))

foreign import ccall "dynamic"
  mkMain :: FunPtr (IO Word32) -> IO Word32

foreign import ccall "dynamic"
  mkFoo :: FunPtr (Double -> Double -> IO Double)
        -> Double -> Double -> IO Double

nullResolver :: MangledSymbol -> IO JITSymbol
nullResolver s = putStrLn "nullresolver" >> return (JITSymbol 0 (JITSymbolFlags False False))

resolver :: MangledSymbol -> IRCompileLayer -> MangledSymbol -> IO JITSymbol
resolver testFunc compileLayer symbol
  | symbol == testFunc = do
      funPtr <- wrapTestFunc myTestFuncImpl
      let addr = ptrToWordPtr (castFunPtrToPtr funPtr)
      return (JITSymbol addr (JITSymbolFlags False True))
  | otherwise = findSymbol compileLayer symbol True

tests :: Test
tests =
  testGroup "OrcJit" $ [
      testCase "trivial main" $ do
        withAssembly testModule3 $ \mod -> do
          failInIO $ withHostTargetMachine $ \tm ->
            withObjectLinkingLayer $ \objectLayer ->
              withIRCompileLayer objectLayer tm $ \compileLayer -> do
                withModuleSet
                  compileLayer
                  [mod]
                  (SymbolResolver (flip (findSymbol compileLayer) True) nullResolver) $
                  \moduleSet -> do
                    mainSymbol <- mangleSymbol compileLayer "main"
                    JITSymbol mainFn _ <-
                      findSymbolIn compileLayer moduleSet mainSymbol True
                    result <- mkMain (castPtrToFunPtr (wordPtrToPtr mainFn))
                    result @?= 42
    , testCase "argument passing" $ do
        withAssembly testModule2 $ \mod -> do
          failInIO $ withHostTargetMachine $ \tm ->
            withObjectLinkingLayer $ \objectLayer ->
              withIRCompileLayer objectLayer tm $ \compileLayer -> do
                withModuleSet
                  compileLayer
                  [mod]
                  (SymbolResolver (flip (findSymbol compileLayer) True) nullResolver) $
                  \moduleSet -> do
                    fooSymbol <- mangleSymbol compileLayer "foo"
                    JITSymbol fooFn _ <- findSymbol compileLayer fooSymbol True
                    result <- mkFoo (castPtrToFunPtr (wordPtrToPtr fooFn)) 2 40
                    result @?= 42
    ] ++ if Info.os == "linux"
         then
           [ testCase "main with callback" $ do
               withAssembly testModule1 $ \mod -> do
                 failInIO $ withHostTargetMachine $ \tm ->
                   withObjectLinkingLayer $ \objectLayer ->
                     withIRCompileLayer objectLayer tm $ \compileLayer -> do
                       testFunc <- mangleSymbol compileLayer "testFunc"
                       withModuleSet
                         compileLayer
                         [mod]
                         (SymbolResolver (resolver testFunc compileLayer) nullResolver) $
                         \moduleSet -> do
                           mainSymbol <- mangleSymbol compileLayer "main"
                           JITSymbol mainFn _ <- findSymbol compileLayer mainSymbol True
                           result <- mkMain (castPtrToFunPtr (wordPtrToPtr mainFn))
                           result @?= 42
           ]
         else []
