module LLVM.General.Arbitrary (
 ) where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Test.QuickCheck

import Data.List
import Data.Word
import Data.Ord
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified LLVM.General.AST as A
import qualified LLVM.General.AST.DataLayout as A
import qualified LLVM.General.AST.Constant as A.C
import qualified LLVM.General.AST.Global as A.G
import qualified LLVM.General.AST.AddrSpace as A
import qualified LLVM.General.AST.Float as A
import qualified LLVM.General.AST.FloatingPointPredicate as A
import qualified LLVM.General.AST.IntegerPredicate as A
import qualified LLVM.General.AST.Attribute as A
import qualified LLVM.General.AST.CallingConvention as A.CC
import qualified LLVM.General.AST.Visibility as A.V
import qualified LLVM.General.AST.Linkage as A.L
import qualified LLVM.General.AST.InlineAssembly as A
import qualified LLVM.General.AST.RMWOperation as A

split :: [Int] -> Gen [Int]
split weights = sized $ \n -> do
  rs <- forM weights $ \w -> choose (0, n*(2*w-1))
  let t = sum rs
  return [ if (t == 0) then 0 else r*n `div` t | r <- rs ]

permute :: Gen [a] -> Gen [a]
permute = (=<<) $ (map fst . sortBy (comparing snd) <$>) . mapM ((<$> (arbitrary :: Gen Int)) . (,))

cFrq :: Bool -> Int -> Int
cFrq b frq = if b then frq else 0

factor :: Gen (Int, Int)
factor = sized $ \s -> do
  x <- choose (0, floor ((sqrt (fromIntegral s)) :: Double))
  return (x, s `div` x)

vectorOfS :: Gen a -> Gen [a]
vectorOfS g = do
  (a, b) <- factor
  vectorOf a (resize b g)

newtype Ident = Ident { getIdent :: String } deriving (Eq, Ord, Read, Show)
instance Arbitrary Ident where
  {- character range is chosen to be high enough to get into multibyte utf-8,
     low enough not to torture terminals, which isn't the point -}
  arbitrary = Ident <$> resize 8 (frequency [
    (f, listOf (elements chrs))
    | (f, chrs) <- [
      (10, ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_','-']),
      (1, map toEnum [32 .. 300])
     ]
   ])

arbitraryFloat :: Word32 -> A.FloatingPointFormat -> Gen A.SomeFloat
arbitraryFloat 16 A.IEEE = A.Half <$> arbitrary
arbitraryFloat 32 A.IEEE = A.Single <$> arbitrary
arbitraryFloat 64 A.IEEE = A.Double <$> arbitrary
arbitraryFloat 128 A.IEEE = A.Quadruple <$> arbitrary <*> arbitrary
arbitraryFloat 80 A.DoubleExtended = A.X86_FP80 <$> arbitrary <*> arbitrary
arbitraryFloat 128 A.PairOfFloats = A.PPC_FP128 <$> arbitrary <*> arbitrary

instance Arbitrary A.Module where
  arbitrary = do
    [ globalsSize, typeDefsSize, metadataNodesSize, namedMetadatasSize, moduleInlineAsmsSize ]
      <- split [ 100, 20, 5, 5, 1 ]

    let preGenerateNames :: (Eq a, Arbitrary a) => Int -> Gen (Int, [a])
        preGenerateNames s = do
          (count, s') <- resize s factor
          names <- nub <$> vectorOf count arbitrary
          return (s', names) 
        forPreGen :: Arbitrary a => (Int, [a]) -> (a -> Gen b) -> Gen [b]
        forPreGen (sz, as) f = forM as $ resize sz . f
                 
    globalsNames <- preGenerateNames globalsSize
    typeDefsNames <- preGenerateNames typeDefsSize
--    metadataNodesIDs <- preGenerateNames metadataNodesSize
--    namedMetadatasNames <- preGenerateNames namedMetadatasSize

    let arbitraryStruct = sized $ \sz -> A.StructureType 
          <$> frequency [ (10, return False), (1, return True) ]
          <*> resize (sz - 1) (vectorOfS (arbitraryType False False True))
        arbitraryType :: Bool -> Bool -> Bool -> Gen A.Type
        arbitraryType voidOk fnOk derived = sized $ \sz -> do
          let sub c = cFrq (c && sz > 0)
          frequency [
            (cFrq voidOk 4, return A.VoidType),
            (10, A.IntegerType <$> elements [ 1, 3, 8, 16, 32, 64, 128 ]),
            (cFrq (sz > 0) 5, A.PointerType <$> (resize (sz-1) (arbitraryType False True True)) <*> arbitrary),
            (5, frequency [ 
                 (40, A.FloatingPointType <$> frequency [ (10, elements [32, 64]), (1, elements [16, 128]) ] <*> pure A.IEEE),
                 (1, return $ A.FloatingPointType 128 A.PairOfFloats),
                 (1, return $ A.FloatingPointType 80 A.DoubleExtended)
                ]),
            (sub fnOk 2, do
               [ rtSize, psSize ] <- resize (sz-1) $ split [ 1, 4 ]
               A.FunctionType
                  <$> resize rtSize (arbitraryType True False False)
                  <*> resize psSize (vectorOfS (arbitraryType False False False))
                  <*> arbitrary),
            (sub derived 2, 
               A.VectorType <$> choose (1, 8) <*> resize (sz - 1) (arbitraryType False False False)),
            (sub derived 3, arbitraryStruct),
            (sub derived 3, 
               A.ArrayType 
                  <$> frequency [ (1, pure 0), ( 10, choose (1,8)) ]
                  <*> resize (sz-1) (arbitraryType False False True)),
            (cFrq (derived && (not . null. snd $ typeDefsNames)) 3,
               A.NamedTypeReference <$> elements (snd typeDefsNames))
           ]

    namedTypes <- liftM Map.fromList $ forPreGen typeDefsNames $ \n ->
      (n,) <$> frequency [ (10, Just <$> arbitraryStruct), (1, pure Nothing) ]

    let constantOfType :: A.Type -> Gen A.C.Constant
        constantOfType type' = sized $ \sz -> do
          let specialFlag = frequency [ (10, pure False), (1, pure True) ]
              intBinOp = do
                [lsz, rsz] <- resize (sz-1) (split [1,1])
                oneof [
                  elements [ A.C.Add, A.C.Sub, A.C.Mul, A.C.Shl ] <*> specialFlag <*> specialFlag,
                  elements [ A.C.UDiv, A.C.SDiv, A.C.LShr, A.C.AShr ] <*> specialFlag,
                  elements [ A.C.URem, A.C.SRem, A.C.And, A.C.Or, A.C.Xor ]
                 ]
                 <*> resize lsz (constantOfType type')
                 <*> resize rsz (constantOfType type')
              fltBinOp = do
                [lsz, rsz] <- resize (sz-1) (split [1,1])
                elements [ A.C.FAdd, A.C.FSub, A.C.FMul, A.C.FDiv, A.C.FRem ]
                 <*> resize lsz (constantOfType type')
                 <*> resize rsz (constantOfType type')
              typeDependentOptions = case type' of
                A.IntegerType bits -> [
                  (2, A.C.Int bits <$> frequency [ 
                         (cFrq (bits >= 8) 10, choose (0,10)),
                         (1, choose (-(2^(bits-1)), (2^(bits-1)-1))) 
                       ]),
                  (cFrq (sz > 0) 1, intBinOp)
                 ]
                A.PointerType rt as -> [
                  (cFrq (sz > 0) 1, do
                     inds <- choose (1,3)
                     bSz:indSzs <- resize (sz-1) $ split (4 : replicate 1 inds)
                     let g :: Gen (A.Type -> A.Type)
                         g = sized $ \s -> promote $ \t -> frequency [ 
                              (3, A.ArrayType <$> choose (0,3) <*> pure t),
                              (1, A.StructureType 
                                   <$> arbitrary
                                   <*> permute ((t:) <$> resize (s-1) (vectorOfS (arbitraryType False False True))))
                             ]
                     A.C.GetElementPtr False 
                        <$> resize bSz (do
                                         ws <- mapM (\s -> resize s g) (tail indSzs)
                                         constantOfType (A.PointerType (foldr ($) rt ws) as))
                        <*> mapM (\indSz -> resize indSz (constantOfType (A.IntegerType 32))) indSzs)
                 ]
                A.FloatingPointType bits fmt -> [
                  (2, A.C.Float <$> arbitraryFloat bits fmt),
                  (cFrq (sz > 0) 1, fltBinOp)
                 ]
                A.FunctionType _ _ _ -> error "FunctionType constant nyi"
                A.VectorType n' et -> let n = fromIntegral n' in [
                  (cFrq (sz > n) 2, A.C.Vector <$> resize (sz `div` n) (vectorOf n (constantOfType et))),
                  (cFrq (sz > 0 && case type' of A.IntegerType _ -> True; _ -> False) 2, intBinOp),
                  (cFrq (sz > 0 && case type' of A.FloatingPointType _ _ -> True; _ -> False) 2, fltBinOp)
                 ]
                A.StructureType pkd ets -> [ 
                  (cFrq (sz > 0) 1, A.C.Struct pkd <$> resize (sz `div` length ets) (mapM constantOfType ets))
                 ]
                A.ArrayType n' et -> let n = fromIntegral n' in [
                  (cFrq (sz > n) 2, A.C.Array et <$> if n == 0 then pure [] else resize (sz `div` n) (vectorOf n (constantOfType et)))
                 ]
                A.NamedTypeReference n -> map (\t -> (1, constantOfType t)) . maybeToList . join . Map.lookup n $ namedTypes
                  
          frequency $ [ 
            (1, pure $ A.C.Undef type'),
            (10, pure $ A.C.Null type')
           ] ++ (map (first (*20)) typeDependentOptions)
        arbitraryGlobal :: A.Name -> Gen A.Global
        arbitraryGlobal name = sized $ \sz -> do
          let sub c = cFrq (c && sz > 0)
          frequency [
            (10, do
               type' <- arbitraryType False False True
               A.GlobalVariable name
                  <$> arbitrary
                   <*> arbitrary
                   <*> frequency [ (5, pure False), (1, pure True)] 
                   <*> arbitrary
                   <*> frequency [ (5, pure False), (1, pure True)] 
                   <*> arbitrary
                   <*> pure type'
                   <*> frequency [ 
                          (cFrq (sz > 0) 4, Just <$> resize (sz - 1) (constantOfType type')),
                          (1, pure Nothing)
                         ]
                   <*> frequency [
                          (20, pure Nothing),
                          (1, pure $ Just "text")
                         ]
                   <*> frequency [ (20, pure 0), (1, pure 4), (2, pure 8) ])
           ]

    A.Module 
       <$> oneof [ return "<string>", getIdent <$> arbitrary ]
       <*> arbitrary
       <*> elements [ Just "x86_64-unknown-linux", Nothing ]
       <*> (permute . liftM concat . sequence) [
            return (map (\(n,t) -> A.TypeDefinition n t) (Map.toList namedTypes)),
            forPreGen globalsNames $ \n -> 
              A.GlobalDefinition <$> arbitraryGlobal n
            ]
    
instance Arbitrary A.MetadataNodeID where
  arbitrary = A.MetadataNodeID <$> choose (0, 1000)

instance Arbitrary A.DataLayout where
  arbitrary = choose (0,5) >>= \s -> resize s $
    A.DataLayout 
       <$> oneof [ pure Nothing, Just <$> arbitraryBoundedEnum ]
       <*> arbitrary
       <*> (Map.fromList <$> arbitrary)
       <*> (Map.fromList <$> arbitrary)
       <*> elements (Nothing : map (Just . Set.fromList) [ [64], [32,64] ])

instance Arbitrary A.AlignType where arbitrary = arbitraryBoundedEnum
instance Arbitrary A.L.Linkage where arbitrary = arbitraryBoundedEnum
instance Arbitrary A.V.Visibility where arbitrary = arbitraryBoundedEnum

instance Arbitrary A.CC.CallingConvention where 
  arbitrary = elements [ A.CC.C, A.CC.Fast, A.CC.Cold, A.CC.GHC, A.CC.Numbered 42 ]

instance Arbitrary A.AlignmentInfo where
  arbitrary = let sizes = [ 2^n | n <- [3..6] ] in
    A.AlignmentInfo <$> elements sizes <*> elements (Nothing : map Just sizes)

instance Arbitrary A.AddrSpace where
  arbitrary = A.AddrSpace <$> frequency [ (50, return 0), (1, return 1) ]

instance Arbitrary A.Name where
  arbitrary = oneof [ 
               A.UnName <$> frequency [(20, choose (0, 10000)), (1, arbitrary)],
               A.Name . getIdent <$> arbitrary
              ]
