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
import Data.Ratio
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

readBits :: String -> [Word8]
readBits = 
  map (foldr (\b n -> b .|. shiftL n 1) 0 . take 8) . takeWhile (not.null) . iterate (drop 8) . 
  map (\c -> case c of '0' -> 0; '1' -> 1) . filter (`elem` "01") . reverse

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

newtype FBits fs = FBits [Word8]
  deriving (Eq)

instance HasSizes fs => Show (FBits fs) where
  show (FBits ws) = showFloatBits (sizes :: Sizes fs) ws

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
            fbits :: (Storable rf, RealFloat rf) => rf -> IO (FBits fs)
            fbits rf = FBits <$> storeBytes rf
            sz@(Sizes eb fb) = sizes :: Sizes fs
            arbWs = oneof [
                     vectorOf ((eb + fb + 1) `div` 8) arbitrary,
                     toBytes <$> (arbitrary :: Gen (SoftFloat fs))
                    ]
            showBits = showFloatBits sz
            testUnaryMember :: (Eq a, Show a) => String -> (forall rf . (Storable rf, RealFloat rf) => rf -> IO a) -> Test
            testUnaryMember s f = testProperty s $ forAll arbWs $ \ws -> 
              morallyDubiousIOProperty $ do
                soft <- loadBytes ws :: IO (SoftFloat fs)
                fSoft <- f soft
                hard <- loadBytes ws :: IO (HardFloat fs)
                fHard <- f hard
                return $ if fSoft == fHard
                  then succeeded
                  else 
                    failed {
                      reason = "with bytes: " ++ showBits ws ++ "\n"
                                 ++ "->: " ++ show hard ++ ", " ++ show soft ++ "\n"
                                 ++ "expected: " ++ show (fHard) ++ "\n"
                                 ++ "but got:  " ++ show (fSoft) ++ "\n" 
                    }
              
            testBinaryMember :: (Eq a, Show a) => String -> (forall rf . (Storable rf, RealFloat rf) => rf -> rf -> IO a) -> Test
            testBinaryMember s f = testProperty s $ forAll ((,) <$> arbWs <*> arbWs) $ \(ws0, ws1) -> 
              morallyDubiousIOProperty $ do
                soft0 <- loadBytes ws0 :: IO (SoftFloat fs)
                soft1 <- loadBytes ws1 :: IO (SoftFloat fs)
                hard0 <- loadBytes ws0 :: IO (HardFloat fs)
                hard1 <- loadBytes ws1 :: IO (HardFloat fs)
                fSoft <- f soft0 soft1
                fHard <- f hard0 hard1
                return $ if fSoft == fHard
                  then succeeded
                  else 
                    failed {
                      reason = "with bytes: " ++ showBits ws0 ++ ", " ++ showBits ws1 ++ "\n"
                                 ++ "->: " ++ show hard0 ++ ", " ++ show soft0 ++ "\n"
                                 ++ "    " ++ show hard1 ++ ", " ++ show soft1 ++ "\n"
                                 ++ "expected: " ++ show fHard ++ "\n"
                                 ++ "but got:  " ++ show fSoft ++ "\n" 
                    }

          in testGroup (show sz) [
             testGroup "Num" [
               testBinaryMember "+" $ ((.).(.)) fbits (+),
               testBinaryMember "*" $ ((.).(.)) fbits (*),
               testBinaryMember "-" $ ((.).(.)) fbits (-),
               testUnaryMember "negate" (fbits . negate),
               testUnaryMember "abs" $ fbits . \f -> if isNaN f || isNegativeZero f then f else abs f,
               testUnaryMember "signum" $ fbits . \f -> if isNaN f then f else signum f
              ],
             testGroup "Eq" [
               testBinaryMember "==" $ ((.).(.)) return (==),
               testBinaryMember "/=" $ ((.).(.)) return (/=)
              ],
             testGroup "Ord" [
               testBinaryMember "compare" $ ((.).(.)) return compare,
               testBinaryMember "<" $ ((.).(.)) return (<),
               testBinaryMember "<=" $ ((.).(.)) return (<=),
               testBinaryMember ">" $ ((.).(.)) return (>),
               testBinaryMember ">=" $ ((.).(.)) return (>=),
               testBinaryMember "min" $ ((.).(.)) fbits min,
               testBinaryMember "max" $ ((.).(.)) fbits max
              ],
             testGroup "Fractional" [
               testBinaryMember "/" $ ((.).(.)) fbits (/),
               testUnaryMember "recip" $ fbits . recip
              ],
             testGroup "RealFrac" $ 
              let boring :: forall rf a . (Storable rf, RealFloat rf) => (rf -> IO a) -> (rf -> IO a)
                  boring = (. \f -> if (isNaN f || isInfinite f) then 0 else f)
              in [
               testUnaryMember "properFraction" $ boring $ \f -> do
                 let (integerPart, fraction) = properFraction f
                 fractionBits <- fbits $ if isNegativeZero fraction then 0 else fraction
                 return (integerPart, fractionBits), 
               testUnaryMember "truncate" $ boring $ return . truncate,
               testUnaryMember "round" $ boring $ return . round,
               testUnaryMember "ceiling" $ boring $ return . ceiling,
               testUnaryMember "floor" $ boring $ return . floor
              ],
             testGroup "Floating" [
               testUnaryMember "exp" $ fbits . exp,
               testUnaryMember "sinh" $ fbits . sinh
              ],
             testGroup "RealFloat" [
               testUnaryMember "floatRadix" $ return . floatRadix,
               testUnaryMember "floatDigits" $ return . floatDigits,
               testUnaryMember "floatRange" $ return . floatRange,
               testUnaryMember "decodeFloat" $ return . \f -> 
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
                                  ++ "expected: " ++ showFloatBits sz hBytes ++ "\n"
                                  ++ "but got:  " ++ showFloatBits sz sBytes ++ "\n" 
                     },

               testUnaryMember "isNaN" $ return . isNaN,
               testUnaryMember "isInfinite" $ return . isInfinite,
               testUnaryMember "isDenormalized" $ return . isDenormalized,
               testUnaryMember "isNegativeZero" $ return . isNegativeZero,
               testUnaryMember "isIEEE" $ return . isNegativeZero
              ]
           ]
    in [
      testSize (FSSingle ()),
      testSize (FSDouble ())
     ],
    testGroup "regressions" [
      testCase "encodeFloat rounding" $ do
        let sz = sizes :: Sizes FSSingle
            f a b s = do
              ws <- storeBytes (encodeFloat a b :: SoftFloat FSSingle)
              showFloatBits sz ws @?= s
  --      f (bit 23 - 1) (-149) "0 00000000 111 1111 1111 1111 1111 1111"
        f (bit 24 - 1) (-150) "0 00000001 000 0000 0000 0000 0000 0000",
  --      f 3 (-151) "0 00000000 000 0000 0000 0000 0000 0001",
      testCase "Add rounding" $ do
        let sz = sizes :: Sizes FSSingle
            l :: Storable s => String -> IO s
            l = loadBytes . readBits 
            pl :: (RealFloat f, Storable f) => IO f
            pl = do
              a <- l "0 00000000 000 0000 0000 0000 0000 0001"
              b <- l "0 00000010 000 0000 0000 0000 0000 0001"
              return $ a + b
        h <- storeBytes =<< (pl :: IO (HardFloat FSSingle))
        s <- storeBytes =<< (pl :: IO (SoftFloat FSSingle))
        showFloatBits sz s @?= showFloatBits sz h,
      testCase "Subtract" $ do
        let sz = sizes :: Sizes FSSingle
            l :: Storable s => String -> IO s
            l = loadBytes . readBits 
            pl :: (RealFloat f, Storable f) => IO f
            pl = do
              a <- l "0 00000100 000 0000 0000 0000 0000 0000"
              b <- l "0 00000000 000 0000 0000 0000 0000 0001"
              return $ a - b
        h <- storeBytes =<< (pl :: IO (HardFloat FSSingle))
        s <- storeBytes =<< (pl :: IO (SoftFloat FSSingle))
        showFloatBits sz s @?= showFloatBits sz h,
      testCase "Multiply rounding" $ do
        let sz = sizes :: Sizes FSSingle
            l :: Storable s => String -> IO s
            l = loadBytes . readBits 
            pl :: (RealFloat f, Storable f) => IO f
            pl = do
              a <- l "0 01011001 001 0000 1110 0110 0111 0000"
              b <- l "1 00001111 011 1100 0110 0100 0001 0110"
              return $ a * b
        h <- storeBytes =<< (pl :: IO (HardFloat FSSingle))
        s <- storeBytes =<< (pl :: IO (SoftFloat FSSingle))
        showFloatBits sz s @?= showFloatBits sz h,
      testCase "Multiply infinity" $ do
        let sz = sizes :: Sizes FSSingle
            l :: Storable s => String -> IO s
            l = loadBytes . readBits 
            pl :: (RealFloat f, Storable f) => IO f
            pl = do
              a <- l "0 11111111 000 0000 0000 0000 0000 0000"
              b <- l "0 11111111 000 0000 0000 0000 0000 0000"
              return $ a * b
        h <- storeBytes =<< (pl :: IO (HardFloat FSSingle))
        s <- storeBytes =<< (pl :: IO (SoftFloat FSSingle))
        showFloatBits sz s @?= showFloatBits sz h,
      testCase "Divide rounding" $ do
        let sz = sizes :: Sizes FSSingle
            l :: Storable s => String -> IO s
            l = loadBytes . readBits 
            pl :: (RealFloat f, Storable f) => IO f
            pl = do
              a <- l "0 01100010 110 0010 0111 0011 0111 0100"
              b <- l "0 00000000 000 0010 0000 0010 0000 0010"
              return $ a / b
        h <- storeBytes =<< (pl :: IO (HardFloat FSSingle))
        s <- storeBytes =<< (pl :: IO (SoftFloat FSSingle))
        showFloatBits sz s @?= showFloatBits sz h,
      testGroup "fromRational" [
        testCase name $ do
          let sz = sizes :: Sizes FSSingle
              pl :: Fractional f => f
              pl = fromRational rat
          h <- storeBytes (pl :: HardFloat FSSingle)
          s <- storeBytes (pl :: SoftFloat FSSingle)
          showFloatBits sz s @?= showFloatBits sz h
        | (name, rat) <- zip (map show [0..]) [
            (bit 141 + 1) % bit 141,
            1271433020295 % 123792455370
          ]
       ],
      testGroup "exp" [
        testGroup "single" [
          testCase name $ do
            let sz = sizes :: Sizes FSSingle
                l :: Storable s => String -> IO s
                l = loadBytes . readBits 
                pl :: (RealFloat f, Storable f) => IO f
                pl = do
                  a <- l bits
                  return $ exp a
            h <- storeBytes =<< (pl :: IO (HardFloat FSSingle))
            s <- storeBytes =<< (pl :: IO (SoftFloat FSSingle))
            showFloatBits sz s @?= showFloatBits sz h
          | (name, bits) <- zip (map show [0..]) [
              "0 00000000 000 0000 0000 0001 0000 0000",
              "0 11010111 110 1110 1100 1110 1101 0000",
              "1 01010011 110 1111 1111 1110 1011 1001",
              "0 10000010 101 0111 0000 0110 0000 1100",
              "1 10000001 001 0010 0010 0000 1010 0001",
              "0 10000000 001 0101 0001 0011 0010 0101",
              "1 10000110 011 1011 0010 1110 1111 0111",
              "1 01111111 111 0010 1011 1011 0101 1100" -- this case hits an OS X expf rounding bug
            ]
         ],
        testGroup "double" [
          testCase name $ do
            let sz = sizes :: Sizes FSDouble
                l :: Storable s => String -> IO s
                l = loadBytes . readBits 
                pl :: (RealFloat f, Storable f) => IO f
                pl = do
                  a <- l bits
                  return $ exp a
            h <- storeBytes =<< (pl :: IO (HardFloat FSDouble))
            s <- storeBytes =<< (pl :: IO (SoftFloat FSDouble))
            showFloatBits sz s @?= showFloatBits sz h
          | (name, bits) <- zip (map show [0..]) [
            ]
         ]
       ]
     ]
 ]
