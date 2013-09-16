{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
module Numeric.Test.SoftFloat where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck.Property
import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary

import Control.Applicative

import Foreign.Marshal
import Foreign.Storable
import Foreign.Ptr

import Numeric
import Data.Word
import Data.Bits
import Data.List
import Numeric.SoftFloat

storeBytes :: Storable s => s -> IO [Word8]
storeBytes s = alloca $ \p -> do
  poke p s
  peekArray (sizeOf s) (castPtr p)

loadBytes :: Storable s => [Word8] -> IO s
loadBytes ws = alloca $ \p -> do
  pokeArray (castPtr p) ws
  peek p

showFloatBits :: Sizes fs -> [Word8] -> String
showFloatBits (Sizes eb fb) ws = 
  let bits = concatMap (\w -> map (\n -> if testBit w n then '1' else '0') [0..7]) ws
      (f,rest) = splitAt fb bits
      (ex,sign) = splitAt eb rest
  in
    sign ++ " " ++ reverse ex ++ " " ++ (
      intercalate " " . map reverse . reverse . takeWhile (not.null) . map (take 4) . iterate (drop 4) $ f
     )

floatlyClose :: [Word8] -> [Word8] -> Bool
floatlyClose x y = 
  let i :: [Word8] -> Integer
      i = foldr (\w i -> i*256 + fromIntegral w) 0
  in 
    x == y || abs (i x - i y) <= 1

instance HasSizes fs => Arbitrary (SoftFloat fs) where
  arbitrary = do
    let Sizes eb fb = sizes :: Sizes fs
        m = 2^(eb - 1)
        n = 2^fb
    SoftFloat <$> arbitrary <*> frequency [
      (100, Normal <$> choose (2 - m, m - 1) <*> choose (0, n - 1)),
      (10, pure Zero),
      (10, Denormal <$> choose (1, n - 1)),
      (10, pure Infinity),
      (5, NaN True <$> choose (0, (n `div` 2) - 1)),
      (5, NaN False <$> choose (1, (n `div` 2) - 1))
     ] 

tests = testGroup "SoftFloat" [
  testGroup "like hardware" $
    let testSize :: forall fs . (
                      Show (HardFloat fs), 
                      RealFloat (HardFloat fs),
                      Storable (HardFloat fs), 
                      HasSizes fs
                    ) => fs -> Test
        testSize fs = 
          let
            sz@(Sizes eb fb) = sizes :: Sizes fs
            testMember :: (Eq a, Show a) => String -> (forall rf . RealFloat rf => rf -> a) -> Test
            testMember s f = testProperty s $ forAll (vectorOf ((eb + fb + 1) `div` 8) arbitrary) $ \ws -> 
              morallyDubiousIOProperty $ do
                soft <- loadBytes ws :: IO (SoftFloat fs)
                hard <- loadBytes ws :: IO (HardFloat fs)
                return $ if f soft == f hard
                  then succeeded
                  else 
                    failed {
                      reason = "with bytes: " ++ showFloatBits sz ws ++ "\n"
                                 ++ "->: " ++ show hard ++ ", " ++ show soft ++ "\n"
                                 ++ "expected: " ++ show (f hard) ++ "\n"
                                 ++ "but got:  " ++ show (f soft) ++ "\n" 
                    }
              
          in testGroup (show (sizes :: Sizes fs)) [
             testGroup "RealFloat" [

               testMember "floatRadix" floatRadix,
               testMember "floatDigits" floatDigits,
               testMember "floatRange" floatRange,
               testMember "decodeFloat" $ \f -> 
                 if (isNaN f || isInfinite f) then Nothing else Just (decodeFloat f),

               testProperty "encodeFloat" $ \s ex -> morallyDubiousIOProperty $ do
                 let soft = encodeFloat s ex :: SoftFloat fs
                     hard = encodeFloat s ex :: HardFloat fs
                 sBytes <- storeBytes soft
                 hBytes <- storeBytes hard
                 return $ if abs ex > 2^20 || floatlyClose sBytes hBytes
                   then 
                     succeeded
                   else
                     failed {
                       reason = "with s=" ++ show s ++ ", ex=" ++ show ex ++ "\n"
                                  ++ "for: " ++ show hard ++ " got " ++ show soft ++ "\n"
                                  ++ "expected: " ++ showFloatBits (sizes :: Sizes fs) hBytes ++ "\n"
                                  ++ "but got:  " ++ showFloatBits (sizes :: Sizes fs) sBytes ++ "\n" 
                     }
             ]
           ]
    in [
      testSize (FSSingle ()),
      testSize (FSDouble ())
     ]
 ]
