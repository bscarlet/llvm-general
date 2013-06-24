module LLVM.General.Test.Instructions where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import LLVM.General.Test.Support

import Control.Monad
import Data.Functor
import Data.Maybe
import Foreign.Ptr
import Data.Word

import LLVM.General.Context
import LLVM.General.Module
import LLVM.General.Diagnostic
import LLVM.General.AST
import LLVM.General.AST.Type
import LLVM.General.AST.Name
import LLVM.General.AST.AddrSpace
import qualified LLVM.General.AST.IntegerPredicate as IPred
import qualified LLVM.General.AST.FloatingPointPredicate as FPPred
import qualified LLVM.General.AST.Linkage as L
import qualified LLVM.General.AST.Visibility as V
import qualified LLVM.General.AST.CallingConvention as CC
import qualified LLVM.General.AST.Attribute as A
import qualified LLVM.General.AST.Global as G
import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.RMWOperation as RMWOp

tests = testGroup "Instructions" [
  testGroup "regular" [
    testCase name $ do
      let mAST = Module "<string>" Nothing Nothing [
            GlobalDefinition $ Function L.External V.Default CC.C [] (VoidType) (UnName 0) ([
                  Parameter (IntegerType 32) (UnName 0) [],
                  Parameter (FloatingPointType 32 IEEE) (UnName 1) [],
                  Parameter (PointerType (IntegerType 32) (AddrSpace 0)) (UnName 2) [],
                  Parameter (IntegerType 64) (UnName 3) [],
                  Parameter (IntegerType 1) (UnName 4) [],
                  Parameter (VectorType 2 (IntegerType 32)) (UnName 5) [],
                  Parameter (StructureType False [IntegerType 32, IntegerType 32]) (UnName 6) []
                 ],False) [] Nothing 0 [
              BasicBlock (UnName 7) [
                namedInstr
               ] (
                Do $ Ret Nothing []
               )
             ]
            ]
          mStr = "; ModuleID = '<string>'\n\
                 \\n\
                 \define void @0(i32, float, i32*, i64, i1, <2 x i32>, { i32, i32 }) {\n\
                 \  " ++ namedInstrS ++ "\n\
                 \  ret void\n\
                 \}\n"
      strCheck mAST mStr
    | let a = LocalReference . UnName,
      (name, namedInstr, namedInstrS) <- (
        [
         (name, UnName 8 := instr, "%8 = " ++ instrS)
         | (name, instr, instrS) <- [
          ("add",
           Add {
             nsw = False,
             nuw = False,
             operand0 = a 0,
             operand1 = a 0,
             metadata = [] 
           },
           "add i32 %0, %0"),
          ("nsw",
           Add {
             nsw = True,
             nuw = False,
             operand0 = a 0,
             operand1 = a 0,
             metadata = [] 
           },
           "add nsw i32 %0, %0"),
          ("nuw",
           Add {
             nsw = False,
             nuw = True,
             operand0 = a 0,
             operand1 = a 0,
             metadata = [] 
           },
           "add nuw i32 %0, %0"),
          ("fadd",
           FAdd {
             operand0 = a 1,
             operand1 = a 1,
             metadata = [] 
           },
           "fadd float %1, %1"),
          ("sub",
           Sub {
             nsw = False,
             nuw = False,
             operand0 = a 0,
             operand1 = a 0,
             metadata = [] 
           },
           "sub i32 %0, %0"),
          ("fsub",
           FSub {
             operand0 = a 1,
             operand1 = a 1,
             metadata = [] 
           },
           "fsub float %1, %1"),
          ("mul",
           Mul {
             nsw = False,
             nuw = False,
             operand0 = a 0,
             operand1 = a 0,
             metadata = [] 
           },
           "mul i32 %0, %0"),
          ("fmul",
           FMul {
             operand0 = a 1,
             operand1 = a 1,
             metadata = [] 
           },
           "fmul float %1, %1"),
          ("udiv",
           UDiv {
             exact = False,
             operand0 = a 0,
             operand1 = a 0,
             metadata = [] 
           },
           "udiv i32 %0, %0"),
          ("exact",
           UDiv {
             exact = True,
             operand0 = a 0,
             operand1 = a 0,
             metadata = [] 
           },
           "udiv exact i32 %0, %0"),
          ("sdiv",
           SDiv {
             exact = False,
             operand0 = a 0,
             operand1 = a 0,
             metadata = [] 
           },
           "sdiv i32 %0, %0"),
          ("fdiv",
           FDiv {
             operand0 = a 1,
             operand1 = a 1,
             metadata = [] 
           },
           "fdiv float %1, %1"),
          ("urem",
           URem {
             operand0 = a 0,
             operand1 = a 0,
             metadata = [] 
           },
           "urem i32 %0, %0"),
          ("srem",
           SRem {
             operand0 = a 0,
             operand1 = a 0,
             metadata = [] 
           },
           "srem i32 %0, %0"),
          ("frem",
           FRem {
             operand0 = a 1,
             operand1 = a 1,
             metadata = [] 
           },
           "frem float %1, %1"),
          ("shl",
           Shl {
             nsw = False,
             nuw = False,
             operand0 = a 0,
             operand1 = a 0,
             metadata = [] 
           },
           "shl i32 %0, %0"),
          ("ashr",
           AShr {
             exact = False,
             operand0 = a 0,
             operand1 = a 0,
             metadata = [] 
           },
           "ashr i32 %0, %0"),
          ("lshr",
           LShr {
             exact = False,
             operand0 = a 0,
             operand1 = a 0,
             metadata = [] 
           },
           "lshr i32 %0, %0"),
          ("and",
           And {
             operand0 = a 0,
             operand1 = a 0,
             metadata = [] 
           },
           "and i32 %0, %0"),
          ("or",
           Or {
             operand0 = a 0,
             operand1 = a 0,
             metadata = [] 
           },
           "or i32 %0, %0"),
          ("xor",
           Xor {
             operand0 = a 0,
             operand1 = a 0,
             metadata = [] 
           },
           "xor i32 %0, %0"),

          ("alloca",
           Alloca {
             allocatedType = IntegerType 32,
             numElements = Nothing,
             alignment = 0,
             metadata = [] 
           },
           "alloca i32"),
          ("load",
           Load {
             volatile = False,
             address = a 2,
             maybeAtomicity = Nothing,
             alignment = 0,
             metadata = [] 
           },
           "load i32* %2"),
          ("volatile",
           Load {
             volatile = True,
             address = a 2,
             maybeAtomicity = Nothing,
             alignment = 0,
             metadata = [] 
           },
           "load volatile i32* %2"),
          ("acquire",
           Load {
             volatile = False,
             address = a 2,
             maybeAtomicity = Just (Atomicity { crossThread = True, memoryOrdering = Acquire }),
             alignment = 1,
             metadata = [] 
           },
           "load atomic i32* %2 acquire, align 1"),
          ("singlethread",
           Load {
             volatile = False,
             address = a 2,
             maybeAtomicity = Just (Atomicity { crossThread = False, memoryOrdering = Monotonic }),
             alignment = 1,
             metadata = [] 
           },
           "load atomic i32* %2 singlethread monotonic, align 1"),
          ("GEP",
           GetElementPtr {
             inBounds = False,
             address = a 2,
             indices = [ a 0 ],
             metadata = [] 
           },
           "getelementptr i32* %2, i32 %0"),
          ("inBounds",
           GetElementPtr {
             inBounds = True,
             address = a 2,
             indices = [ a 0 ],
             metadata = [] 
           },
           "getelementptr inbounds i32* %2, i32 %0"),
          ("cmpxchg",
           CmpXchg {
             volatile = False,
             address = a 2,
             expected = a 0,
             replacement = a 0,
             atomicity = Atomicity { crossThread = True, memoryOrdering = Monotonic },
             metadata = [] 
           },
           "cmpxchg i32* %2, i32 %0, i32 %0 monotonic"),
          ("atomicrmw",
           AtomicRMW {
             volatile = False,
             rmwOperation = RMWOp.UMax,
             address = a 2,
             value = a 0,
             atomicity = Atomicity { crossThread = True, memoryOrdering = Release },
             metadata = []
           },
           "atomicrmw umax i32* %2, i32 %0 release"),

          ("trunc",
           Trunc {
             operand0 = a 0,
             type' = IntegerType 16,
             metadata = [] 
           },
           "trunc i32 %0 to i16"),
          ("zext",
           ZExt {
             operand0 = a 0,
             type' = IntegerType 64,
             metadata = [] 
           },
           "zext i32 %0 to i64"),
          ("sext",
           SExt {
             operand0 = a 0,
             type' = IntegerType 64,
             metadata = [] 
           },
           "sext i32 %0 to i64"),
          ("fptoui",
           FPToUI {
             operand0 = a 1,
             type' = IntegerType 64,
             metadata = [] 
           },
           "fptoui float %1 to i64"),
          ("fptosi",
           FPToSI {
             operand0 = a 1,
             type' = IntegerType 64,
             metadata = [] 
           },
           "fptosi float %1 to i64"),
          ("uitofp",
           UIToFP {
             operand0 = a 0,
             type' = FloatingPointType 32 IEEE,
             metadata = [] 
           },
           "uitofp i32 %0 to float"),
          ("sitofp",
           SIToFP {
             operand0 = a 0,
             type' = FloatingPointType 32 IEEE,
             metadata = [] 
           },
           "sitofp i32 %0 to float"),
          ("fptrunc",
           FPTrunc {
             operand0 = a 1,
             type' = FloatingPointType 16 IEEE,
             metadata = [] 
           },
           "fptrunc float %1 to half"),
          ("fpext",
           FPExt {
             operand0 = a 1,
             type' = FloatingPointType 64 IEEE,
             metadata = [] 
           },
           "fpext float %1 to double"),
          ("ptrtoint",
           PtrToInt {
             operand0 = a 2,
             type' = IntegerType 32,
             metadata = [] 
           },
           "ptrtoint i32* %2 to i32"),
          ("inttoptr",
           IntToPtr {
             operand0 = a 0,
             type' = PointerType (IntegerType 32) (AddrSpace 0),
             metadata = [] 
           },
           "inttoptr i32 %0 to i32*"),
          ("bitcast",
           BitCast {
             operand0 = a 0,
             type' = FloatingPointType 32 IEEE,
             metadata = [] 
           },
           "bitcast i32 %0 to float"),
          ("select",
           Select {
             condition' = a 4,
             trueValue = a 0,
             falseValue = a 0,
             metadata = []
           },
           "select i1 %4, i32 %0, i32 %0"),
          ("vaarg",
           VAArg {
             argList = a 2,
             type' = IntegerType 16,
             metadata = []
           },
           "va_arg i32* %2, i16"),
          ("extractelement",
           ExtractElement {
             vector = a 5,
             index = a 0,
             metadata = []
           },
           "extractelement <2 x i32> %5, i32 %0"),
          ("insertelement",
           InsertElement {
             vector = a 5,
             element = a 0,
             index = a 0,
             metadata = []
           },
           "insertelement <2 x i32> %5, i32 %0, i32 %0"),
          ("shufflevector",
           ShuffleVector {
             operand0 = a 5,
             operand1 = a 5,
             mask = C.Vector [ C.Int 32 p | p <- [0..1] ],
             metadata = []
           },
           "shufflevector <2 x i32> %5, <2 x i32> %5, <2 x i32> <i32 0, i32 1>"),
          ("extractvalue",
           ExtractValue {
             aggregate = a 6,
             indices' = [0],
             metadata = []
           },
           "extractvalue { i32, i32 } %6, 0"),
          ("insertvalue",
           InsertValue {
             aggregate = a 6,
             element = a 0,
             indices' = [0],
             metadata = []
           },
           "insertvalue { i32, i32 } %6, i32 %0, 0")
         ] ++ [
          ("landingpad-" ++ n,
           LandingPad {
             type' = StructureType False [ 
                PointerType (IntegerType 8) (AddrSpace 0),
                IntegerType 32
               ],
             personalityFunction = ConstantOperand (C.GlobalReference (UnName 0)),
             cleanup = cp,
             clauses = cls,
             metadata = []
           },
           "landingpad { i8*, i32 } personality void (i32, float, i32*, i64, i1, <2 x i32>, { i32, i32 })* @0" ++ s)
          | (clsn,cls,clss) <- [
           ("catch",
            [Catch (C.Null (PointerType (IntegerType 8) (AddrSpace 0)))],
            "\n          catch i8* null"),
           ("filter",
            [Filter (C.Null (ArrayType 1 (PointerType (IntegerType 8) (AddrSpace 0))))],
            "\n          filter [1 x i8*] zeroinitializer")
          ],
          (cpn, cp, cps) <- [ ("-cleanup", True, "\n          cleanup"), ("", False, "") ],
          let s = cps ++ clss
              n = clsn ++ cpn
         ] ++ [
          ("icmp-" ++ ps,
           ICmp { iPredicate = p, operand0 = a 0, operand1 = a 0, metadata = [] },
           "icmp " ++ ps ++ " i32 %0, %0")
           | (ps, p) <- [
           ("eq", IPred.EQ),
           ("ne", IPred.NE),
           ("ugt", IPred.UGT),
           ("uge", IPred.UGE),
           ("ult", IPred.ULT),
           ("ule", IPred.ULE),
           ("sgt", IPred.SGT),
           ("sge", IPred.SGE),
           ("slt", IPred.SLT),
           ("sle", IPred.SLE)
          ]
         ] ++ [
          ("fcmp-" ++ ps,
           FCmp { fpPredicate = p, operand0 = a 1, operand1 = a 1, metadata = [] },
           "fcmp " ++ ps ++ " float %1, %1")
           | (ps, p) <- [
           ("false", FPPred.False),
           ("oeq", FPPred.OEQ),
           ("ogt", FPPred.OGT),
           ("oge", FPPred.OGE),
           ("olt", FPPred.OLT),
           ("ole", FPPred.OLE),
           ("one", FPPred.ONE),
           ("ord", FPPred.ORD),
           ("uno", FPPred.UNO),
           ("ueq", FPPred.UEQ),
           ("ugt", FPPred.UGT),
           ("uge", FPPred.UGE),
           ("ult", FPPred.ULT),
           ("ule", FPPred.ULE),
           ("une", FPPred.UNE),
           ("true", FPPred.True)
          ]
         ]
        ] ++ [
         ("store",
          Do $ Store {
            volatile = False,
            address = a 2,
            value = a 0,
            maybeAtomicity = Nothing,
            alignment = 0,
            metadata = [] 
          },
          "store i32 %0, i32* %2"),
         ("fence",
          Do $ Fence {
            atomicity = Atomicity { crossThread = True, memoryOrdering = Acquire },
            metadata = [] 
          },
          "fence acquire"),
          ("call",
           Do $ Call {
             isTailCall = False,
             callingConvention = CC.C,
             returnAttributes = [],
             function = Right (ConstantOperand (C.GlobalReference (UnName 0))),
             arguments = [ (LocalReference (UnName i), []) | i <- [0..6] ],
             functionAttributes = [],
             metadata = []
           },
           "call void @0(i32 %0, float %1, i32* %2, i64 %3, i1 %4, <2 x i32> %5, { i32, i32 } %6)")
        ]
      )
   ],
  testGroup "terminators" [
    testCase name $ strCheck mAST mStr
    | (name, mAST, mStr) <- [
     (
       "ret",
       Module "<string>" Nothing Nothing [
        GlobalDefinition $ Function L.External V.Default CC.C [] (VoidType) (UnName 0) ([
             ],False) [] Nothing 0
         [
          BasicBlock (UnName 0) [
           ] (
            Do $ Ret Nothing []
           )
         ]
        ],
       "; ModuleID = '<string>'\n\
       \\n\
       \define void @0() {\n\
       \  ret void\n\
       \}\n"
     ), (
       "br",
       Module "<string>" Nothing Nothing [
        GlobalDefinition $ Function L.External V.Default CC.C [] (VoidType) (UnName 0) ([
             ],False) [] Nothing 0
         [
          BasicBlock (UnName 0) [] (
            Do $ Br (Name "foo") []
           ),
          BasicBlock (Name "foo") [] (
            Do $ Ret Nothing []
           )
         ]
        ],
       "; ModuleID = '<string>'\n\
       \\n\
       \define void @0() {\n\
       \  br label %foo\n\
       \\n\
       \foo:                                              ; preds = %0\n\
       \  ret void\n\
       \}\n"
     ), (
       "condbr",
       Module "<string>" Nothing Nothing [
        GlobalDefinition $ Function L.External V.Default CC.C [] (VoidType) (UnName 0) ([
             ],False) [] Nothing 0
         [
          BasicBlock (Name "bar") [] (
            Do $ CondBr (ConstantOperand (C.Int 1 1)) (Name "foo") (Name "bar") []
           ),
          BasicBlock (Name "foo") [] (
            Do $ Ret Nothing []
           )
         ]
        ],
       "; ModuleID = '<string>'\n\
       \\n\
       \define void @0() {\n\
       \bar:\n\
       \  br i1 true, label %foo, label %bar\n\
       \\n\
       \foo:                                              ; preds = %bar\n\
       \  ret void\n\
       \}\n"
     ), (
       "switch",
       Module "<string>" Nothing Nothing [
        GlobalDefinition $ Function L.External V.Default CC.C [] (VoidType) (UnName 0) ([
             ],False) [] Nothing 0
         [
          BasicBlock (UnName 0) [] (
            Do $ Switch {
              operand0' = ConstantOperand (C.Int 16 2),
              defaultDest = Name "foo",
              dests = [
               (C.Int 16 0, UnName 0),
               (C.Int 16 2, Name "foo"),
               (C.Int 16 3, UnName 0)
              ],
              metadata' = []
           }
          ),
          BasicBlock (Name "foo") [] (
            Do $ Ret Nothing []
           )
         ]
        ],
       "; ModuleID = '<string>'\n\
       \\n\
       \define void @0() {\n\
       \; <label>:0\n\
       \  switch i16 2, label %foo [\n\
       \    i16 0, label %0\n\
       \    i16 2, label %foo\n\
       \    i16 3, label %0\n\
       \  ]\n\
       \\n\
       \foo:                                              ; preds = %0, %0\n\
       \  ret void\n\
       \}\n"
     ), (
       "indirectbr",
       Module "<string>" Nothing Nothing [
        GlobalDefinition $ globalVariableDefaults {
          G.name = UnName 0,
          G.type' = PointerType (IntegerType 8) (AddrSpace 0),
          G.initializer = Just (C.BlockAddress (Name "foo") (UnName 2))
        },
        GlobalDefinition $ Function L.External V.Default CC.C [] (VoidType) (Name "foo") ([
             ],False) [] Nothing 0
         [
          BasicBlock (UnName 0) [
            UnName 1 := Load {
                     volatile = False,
                     address = ConstantOperand (C.GlobalReference (UnName 0)),
                     maybeAtomicity = Nothing,
                     alignment = 0,
                     metadata = [] 
                   }
          ] (
            Do $ IndirectBr {
              operand0' = LocalReference (UnName 1),
              possibleDests = [UnName 2],
              metadata' = []
           }
          ),
          BasicBlock (UnName 2) [] (
            Do $ Ret Nothing []
           )
         ]
        ],
--       \  indirectbr i8* null, [label %foo]\n\
       "; ModuleID = '<string>'\n\
       \\n\
       \@0 = global i8* blockaddress(@foo, %2)\n\
       \\n\
       \define void @foo() {\n\
       \  %1 = load i8** @0\n\
       \  indirectbr i8* %1, [label %2]\n\
       \\n\
       \; <label>:2                                       ; preds = %0\n\
       \  ret void\n\
       \}\n"
     ), (
       "invoke",
       Module "<string>" Nothing Nothing [
        GlobalDefinition $ Function L.External V.Default CC.C [] (VoidType) (UnName 0) ([
                  Parameter (IntegerType 32) (UnName 0) [],
                  Parameter (IntegerType 16) (UnName 1) []
             ],False) [] Nothing 0
         [
          BasicBlock (UnName 2) [] (
            Do $ Invoke {
             callingConvention' = CC.C,
             returnAttributes' = [],
             function' = Right (ConstantOperand (C.GlobalReference (UnName 0))),
             arguments' = [
              (ConstantOperand (C.Int 32 4), []),
              (ConstantOperand (C.Int 16 8), [])
             ],
             functionAttributes' = [],
             returnDest = Name "foo",
             exceptionDest = Name "bar",
             metadata' = []
            }
           ),
          BasicBlock (Name "foo") [] (
            Do $ Ret Nothing []
           ),
          BasicBlock (Name "bar") [
           UnName 3 := LandingPad {
             type' = StructureType False [ 
                PointerType (IntegerType 8) (AddrSpace 0),
                IntegerType 32
               ],
             personalityFunction = ConstantOperand (C.GlobalReference (UnName 0)),
             cleanup = True,
             clauses = [Catch (C.Null (PointerType (IntegerType 8) (AddrSpace 0)))],
             metadata = []
           }
           ] (
            Do $ Ret Nothing []
           )
         ]
        ],
       "; ModuleID = '<string>'\n\
       \\n\
       \define void @0(i32, i16) {\n\
       \  invoke void @0(i32 4, i16 8)\n\
       \          to label %foo unwind label %bar\n\
       \\n\
       \foo:                                              ; preds = %2\n\
       \  ret void\n\
       \\n\
       \bar:                                              ; preds = %2\n\
       \  %3 = landingpad { i8*, i32 } personality void (i32, i16)* @0\n\
       \          cleanup\n\
       \          catch i8* null\n\
       \  ret void\n\
       \}\n"
     ), (
       "resume",
       Module "<string>" Nothing Nothing [
        GlobalDefinition $ Function L.External V.Default CC.C [] (VoidType) (UnName 0) ([
             ],False) [] Nothing 0
         [
          BasicBlock (UnName 0) [] (
            Do $ Resume (ConstantOperand (C.Int 32 1)) []
           )
         ]
        ],
       "; ModuleID = '<string>'\n\
       \\n\
       \define void @0() {\n\
       \  resume i32 1\n\
       \}\n"
     ), (
       "unreachable",
       Module "<string>" Nothing Nothing [
        GlobalDefinition $ Function L.External V.Default CC.C [] (VoidType) (UnName 0) ([
             ],False) [] Nothing 0
         [
          BasicBlock (UnName 0) [] (
            Do $ Unreachable []
           )
         ]
        ],
       "; ModuleID = '<string>'\n\
       \\n\
       \define void @0() {\n\
       \  unreachable\n\
       \}\n"
     )
    ]
   ]
 ]
