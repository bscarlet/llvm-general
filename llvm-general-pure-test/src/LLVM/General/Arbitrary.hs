module LLVM.General.Arbitrary (
 ) where

import Control.Applicative
import Control.Monad
import Test.QuickCheck

import Data.List
import Data.Ord
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified LLVM.General.AST as A
import qualified LLVM.General.AST.DataLayout as A
import qualified LLVM.General.AST.Constant as A.C
import qualified LLVM.General.AST.AddrSpace as A
import qualified LLVM.General.AST.Float as A
import qualified LLVM.General.AST.FloatingPointPredicate as A
import qualified LLVM.General.AST.IntegerPredicate as A
import qualified LLVM.General.AST.Attribute as A
import qualified LLVM.General.AST.CallingConvention as A
import qualified LLVM.General.AST.Visibility as A
import qualified LLVM.General.AST.Linkage as A
import qualified LLVM.General.AST.InlineAssembly as A
import qualified LLVM.General.AST.RMWOperation as A

split :: [Int] -> Gen [Int]
split weights = sized $ \n -> do
  rs <- forM weights $ \w -> choose (0, n*(2*w-1))
  let t = sum rs
  return [ if (t == 0) then 0 else r*n `div` t | r <- rs ]

permute :: Gen [a] -> Gen [a]
permute = (=<<) $ (map fst . sortBy (comparing snd) <$>) . mapM ((<$> (arbitrary :: Gen Int)) . (,))

szFrq :: Int -> Int -> Int
szFrq sz frq = if sz > 0 then frq else 0

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

instance Arbitrary A.Module where
  arbitrary = do
    [ globalsSize, typeDefsSize, metadataNodesSize, namedMetadatasSize, moduleInlineAsmsSize ]
      <- split [ 100, 20, 5, 5, 1 ]

    let preGenerateNames :: (Eq a, Arbitrary a) => Int -> Gen (Int, [a])
        preGenerateNames s = do
          count <- choose (0, floor ((sqrt (fromIntegral s)) :: Double))
          names <- nub <$> vectorOf count arbitrary
          return (s `div` count, names) 
                 
--    globalsNames <- preGenerateNames globalsSize
    (typeDefSize, typeDefsNames) <- preGenerateNames typeDefsSize
--    metadataNodesIDs <- preGenerateNames metadataNodesSize
--    namedMetadatasNames <- preGenerateNames namedMetadatasSize
    let arbitraryType :: Gen A.Type
        arbitraryType = sized $ \sz -> frequency [
          (4, return A.VoidType),
          (10, A.IntegerType <$> elements [ 1, 3, 8, 16, 32, 64, 128 ]),
          (szFrq sz 5, A.PointerType <$> (resize (sz-1) arbitraryType) <*> arbitrary)
         ]

    A.Module 
       <$> oneof [ return "<string>", getIdent <$> arbitrary ]
       <*> arbitrary
       <*> elements [ Just "x86_64-unknown-linux", Nothing ]
       <*> permute (
          forM typeDefsNames $ \n ->
            A.TypeDefinition n <$> frequency [(10, Just <$> arbitraryType), (1, return Nothing)]
        )
    
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
