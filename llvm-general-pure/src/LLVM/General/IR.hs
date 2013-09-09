module LLVM.General.IR (
 IR(..)
 ) where

import Text.ParserCombinators.Parsec

import Control.Applicative

import Data.Function
import Data.List
import Data.Char
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BSUTF8
import Numeric

import qualified LLVM.General.AST as A
import qualified LLVM.General.AST.Float as A
import qualified LLVM.General.AST.AddrSpace as A
import qualified LLVM.General.AST.Constant as A.C
import qualified LLVM.General.AST.Global as A.G
import qualified LLVM.General.AST.Linkage as A.L
import qualified LLVM.General.AST.Visibility as A.V
import LLVM.General.DataLayout (dataLayoutToString)

class IR a where
  toIR :: a -> String
  toIR _ = "(incomplete toIR)"
  fromIR :: Parser a
  fromIR = error "undefined fromIR"

instance IR A.Module where
--  toIR (A.Module { A.moduleDataLayout = Nothing, A.moduleTargetTriple = Nothing, A.moduleDefinitions = [] }) = ""
  toIR m = 
    (case A.moduleName m of 
       "" -> ""
       s | '\n' `elem` s -> ""
         | otherwise -> "; ModuleID = '" ++ s ++ "'\n")
    ++ maybe "" (\dl -> "target datalayout = \"" ++ dataLayoutToString dl ++ "\"\n") (A.moduleDataLayout m)
    ++ maybe "" (\tr -> "target triple = " ++ show tr ++ "\n") (A.moduleTargetTriple m)
    ++ (
      let parts = groupBy ((==) `on` part) . sortBy (compare `on` part) . A.moduleDefinitions $ m
            where
              part d = 
                case d of
                  A.GlobalDefinition _ -> 0
                  A.TypeDefinition _ _ -> 1
                  A.MetadataNodeDefinition _ _ -> 2
                  A.NamedMetadataDefinition _ _ -> 3
                  A.ModuleInlineAssembly _ -> 4
      in
        concatMap (\ps -> "\n" ++ concatMap toIR ps) parts
    )
  fromIR = return A.defaultModule

instance IR A.Definition where
  toIR (A.GlobalDefinition g) = toIR g
  toIR (A.TypeDefinition n t) = "%" ++ toIR n ++ maybe "(opaque type)" ((" = " ++) . toIR) t ++ "\n"
  toIR x = "(toIR: " ++ show x ++ ")"

instance IR A.Global where
  toIR g@(A.G.GlobalVariable {}) = 
    "@" ++ toIR (A.G.name g) ++ " ="
      ++ toIR (A.G.linkage g)
      ++ toIR (A.G.visibility g)
      ++ (if A.G.isThreadLocal g then " thread_local" else "")
      ++ toIR (A.G.addrSpace g)
      ++ (if A.G.hasUnnamedAddr g then " unnamed_addr" else "")
      ++ (if A.G.isConstant g then " constant" else " global")
      ++ " " ++ toIR (A.G.type' g)
      ++ maybe "" ((' ':).toIR) (A.G.initializer g)
      ++ maybe "" (\s -> ", section \"" ++ escape s ++ "\"") (A.G.section g)
      ++ (case A.G.alignment g of 0 -> ""; a -> ", align " ++ show g)
      ++ "\n"
  
instance IR A.Type where
  toIR (A.IntegerType b) = "i" ++ show b
  toIR (A.FloatingPointType 32 A.IEEE) = "float"
  toIR (A.FloatingPointType 64 A.IEEE) = "double"
  toIR (A.ArrayType n t) = "[" ++ show n ++ " x " ++ toIR t ++ "]"
  toIR (A.StructureType p ts) = (if p then \s -> "<" ++ s ++ ">" else id) ("{" ++ intercalate ", " (map toIR ts) ++ "}")
  toIR (A.PointerType t as) = toIR t ++ "*"
  toIR x = "(IR type for " ++ show x ++ " nyi)"

class Typed a where
  typeOf :: a -> A.Type

instance Typed A.C.Constant where
  typeOf (A.C.Int b _) = A.IntegerType b
  typeOf (A.C.Float (A.Single _)) = A.FloatingPointType 32 A.IEEE
  typeOf (A.C.Float (A.Double _)) = A.FloatingPointType 64 A.IEEE
  typeOf (A.C.Null t) = t
  typeOf (A.C.GetElementPtr _ p is) = either error id $ do
    let pt = typeOf p                                        
    let indexCheck (A.PointerType _ _) (A.IntegerType _) = return ()
        indexCheck (A.VectorType n (A.PointerType _ _)) (A.VectorType n' (A.IntegerType _)) | n == n' = return ()
        indexCheck _ _ = fail "invalid arguments to getelementptr"
    mapM (indexCheck pt . typeOf) is
    let walk t [] = return t
        walk (A.StructureType p ts) (A.C.Int 32 i : is)
          | i >= 0 && i < fromIntegral (length ts) = walk (ts !! fromIntegral i) is
        walk st@(A.StructureType _ _) (A.C.Vector (it@(A.C.Int 32 i):vs) : is) | all sameInt vs = walk st (it:is)
          where sameInt (A.C.Int 32 i') | i == i' = True
                sameInt _ = False
        walk (A.ArrayType _ t) (_:is) = walk t is
        walk (A.VectorType _ t) (_:is) = walk t is
        walk t _ = fail $ "invalid argument to getelementptr"
    case (is, pt) of
      ([], t) -> return t
      (_:is, A.PointerType t as) -> A.PointerType <$> walk t is <*> pure as
      (_:is, A.VectorType n (A.PointerType t as)) -> A.VectorType n <$> (A.PointerType <$> walk t is <*> pure as)

instance IR A.C.Constant where
  toIR = fst . cToIR
    where cToIR :: A.C.Constant -> (String, A.Type)
          cToIR c = (cToIR' c, typeOf c)
          cToIR' (A.C.Int b v) = show v
          cToIR' (A.C.Float (A.Single x)) = showEFloat Nothing x ""
          cToIR' (A.C.Float (A.Double x)) = showEFloat Nothing x ""
          cToIR' (A.C.Null t) = case t of A.PointerType _ _ -> "null"; _ -> "zeroinitializer"
          cToIR' (A.C.GetElementPtr _ p is) = "getelementptr (" ++ intercalate ", " (map ((\(ir,t) -> toIR t ++ " " ++ ir) . cToIR) (p:is)) ++ ")"
          cToIR' c = "(serialize constant: " ++ show c ++ ")"

escape :: String -> String
escape = concatMap esc . map (toEnum . fromIntegral) . BS.unpack . BSUTF8.fromString
  where
    esc c | isAscii c && isPrint c && c /= '\\' && c /= '"' = [c]
          | otherwise = "\\" ++ map toUpper (showHex (fromEnum c) "")

instance IR A.Name where
  toIR (A.Name n) = 
    let esc = "\"" ++ escape n ++ "\"" in
    case n of
      d:_ | isDigit d -> esc
          | all (\c -> (isAscii c && (isDigit c || isAlpha c)) || c == '-' || c == '.' || c == '_') n -> n
      _ -> esc
  toIR (A.UnName n) = show n

instance IR A.L.Linkage where
  toIR A.L.Private = " private"
  toIR A.L.LinkerPrivate = " linker_private"
  toIR A.L.LinkerPrivateWeak = " linker_private_weak"
  toIR A.L.Internal = " internal"
  toIR A.L.AvailableExternally = " available_externally"
  toIR A.L.LinkOnce = " linkonce"
  toIR A.L.Weak = " weak"
  toIR A.L.Common = " common"
  toIR A.L.Appending = " appending"
  toIR A.L.ExternWeak = " extern_weak"
  toIR A.L.LinkOnceODR = " linkonce_odr"
  toIR A.L.WeakODR = " weak_odr"
  toIR A.L.External = " external"
  toIR A.L.DLLImport = " dllimport"
  toIR A.L.DLLExport = " dllexport"

instance IR A.V.Visibility where
  toIR A.V.Default = ""
  toIR A.V.Hidden = " hidden"
  toIR A.V.Protected = " protected"

instance IR A.AddrSpace where
  toIR (A.AddrSpace 0) = ""
  toIR (A.AddrSpace i) = " addrspace(" ++ show i ++ ")"

