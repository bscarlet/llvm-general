{-# LANGUAGE ScopedTypeVariables #-}
module Numeric.Test.SoftFloat where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck.Property

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

tests = testGroup "SoftFloat" [
  testGroup "like hardware" $
    let testSize :: forall fs . (
                      Show (HardFloat fs), 
                      RealFloat (HardFloat fs),
                      Storable (HardFloat fs), 
                      HasSizes fs
                    ) => fs -> Test
        testSize fs = testGroup (show (sizes :: Sizes fs)) [
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
    in [
      testSize (FSSingle ()),
      testSize (FSDouble ())
     ]
 ]
