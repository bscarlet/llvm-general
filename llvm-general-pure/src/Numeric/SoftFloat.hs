{-# LANGUAGE 
  TypeFamilies,
  ScopedTypeVariables
  #-}
module Numeric.SoftFloat where

import Prelude hiding (exponent)
import Control.Monad

import Debug.Trace

import Data.Bits
import Data.Word

import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable

data Sizes fs = Sizes { exponentBits :: Int, fractionBits :: Int }
  deriving (Read, Show)

class HasSizes fs where
  sizes :: Sizes fs

type family HardFloat fs :: *

newtype FSHalf = FSHalf ()
newtype FSSingle = FSSingle ()
newtype FSDouble = FSDouble ()
newtype FSQuadruple = FSQuadruple ()

type instance HardFloat FSSingle = Float
type instance HardFloat FSDouble = Double

data FloatCase fs
  = Normal { exponent :: Int, fraction :: Integer }
  | Zero
  | Denormal { fraction :: Integer }
  | Infinity
  | NaN { signaling :: Bool, payload :: Integer }
  deriving (Read, Show)

floatCaseToEF :: forall m fs . (Monad m, HasSizes fs) => FloatCase fs -> m (Int, Integer)
floatCaseToEF fc = do
  let Sizes eb fb = sizes :: Sizes fs
      ones = shiftL 1 eb - 1
  case fc of
    Normal ex fr | ex >= 1 - m && ex <= m -> return (ex + m - 1, fr)
                 where m = shiftL 1 (eb - 1)
    Zero -> return (0, 0)
    Denormal s | s /= 0 && shiftR s fb == 0 -> return (0, s)
    Infinity -> return (ones, 0)
    NaN sig pl | (sig || pl /= 0) && (pl >= 0 && shiftR pl (fb - 1) == 0) -> 
      return (ones, if sig then shiftL 1 (fb - 1) else 0 .|. pl)
    _ -> fail $ "malformed float case: " ++ show fc

efToFloatCase :: forall m fs . (Monad m, HasSizes fs) => (Int, Integer) -> m (FloatCase fs)
efToFloatCase(ex, fr) = do
  let Sizes eb fb = sizes :: Sizes fs
  case ex of
    0 -> return $ case fr of 0 -> Zero; fr -> Denormal fr
    _ | ex == ones -> return $ case fr of 0 -> Infinity; pl | shiftR pl fb == 0 -> NaN (testBit pl (fb - 1)) (clearBit pl (fb - 1))
      | shiftR ex eb == 0 && shiftR fr fb == 0 -> return $ Normal (ex - (shiftL 1 (eb - 1)) + 1) fr
      | otherwise -> fail "mailformed input making FloatCase"
        where ones = shiftL 1 eb - 1

data SoftFloat fs = SoftFloat { sign :: Bool, floatCase :: FloatCase fs }
  deriving (Read, Show)

instance HasSizes FSHalf where sizes = Sizes { exponentBits = 5, fractionBits = 10 }
instance HasSizes FSSingle where sizes = Sizes { exponentBits = 8, fractionBits = 23 }
instance HasSizes FSDouble where sizes = Sizes { exponentBits = 11, fractionBits = 52 }
instance HasSizes FSQuadruple where sizes = Sizes { exponentBits = 15, fractionBits = 112 }

integerToBytes :: Int -> Integer -> [Word8]
integerToBytes n i = map (fromIntegral . (.&. 255) . shiftR i) [0,8 .. 8*(n-1)]

integerFromBytes :: [Word8] -> Integer
integerFromBytes [] = 0
integerFromBytes (b:bs) = fromIntegral b .|. (shiftL (integerFromBytes bs) 8)

reverseIfBigEndian = alloca $ \p -> do
  pokeArray (castPtr p) [0 :: Word8 .. 7]
  i <- peek p :: IO Word64
  case i of
    0x0706050403020100 -> return id
    0x0001020304050607 -> return reverse
    _ -> fail "mixed endianness"

toBytes :: forall fs . HasSizes fs => SoftFloat fs -> [Word8]
toBytes sf@(SoftFloat sgn fc) = either error id $ do
  let sz@(Sizes eb fb) = sizes :: Sizes fs
  (ex, fr) <- floatCaseToEF fc
  return . integerToBytes ((eb + fb + 1) `div` 8)
    $ shiftL (shiftL (if sgn then 1 else 0) eb .|. fromIntegral ex) fb .|. fr

fromBytes :: forall fs . HasSizes fs => [Word8] -> SoftFloat fs
fromBytes bytes = either error id $ do
  let sz@(Sizes eb fb) = sizes :: Sizes fs
  let bits = integerFromBytes $ bytes
  when (shiftR bits (eb + fb + 1) /= 0) $ fail "too many bits making SoftFloat"
  fc <- efToFloatCase (
    fromIntegral (clearBit (shiftR bits fb) eb), 
    bits .&. complement (shiftL (-1) fb)
   )
  return SoftFloat {
    sign = testBit bits (eb + fb),
    floatCase = fc
  }

bitWidth s = search (\b -> shiftR s b == 0)
  where
    search :: (Int -> Bool) -> Int
    search p = up 0 0
      where up r b = if p r then down r (b-1) else up (setBit r b) (b+1)
            down r (-1) = r
            down r b = let r' = clearBit r b in down (if p r' then r' else r) (b-1)

instance HasSizes fs => Storable (SoftFloat fs) where
  sizeOf _ = let Sizes eb fb = sizes :: Sizes fs
                 (sz, 0) = (eb + fb + 1) `divMod` 8
             in
               sz
  alignment _ = 1
  poke p sf = do
    reverseIfBigEndian <- reverseIfBigEndian
    pokeArray (castPtr p) (reverseIfBigEndian . toBytes $ sf)
  peek p = do
    reverseIfBigEndian <- reverseIfBigEndian
    liftM (fromBytes . reverseIfBigEndian) (peekArray (sizeOf (undefined :: SoftFloat fs)) (castPtr p))

instance HasSizes fs => Num (SoftFloat fs) where
  abs s = s { sign = False }

instance HasSizes fs => Eq (SoftFloat fs) where

instance HasSizes fs => Ord (SoftFloat fs) where

instance HasSizes fs => Real (SoftFloat fs) where

instance HasSizes fs => Fractional (SoftFloat fs) where

instance HasSizes fs => RealFrac (SoftFloat fs) where

instance HasSizes fs => Floating (SoftFloat fs) where

instance HasSizes fs => RealFloat (SoftFloat fs) where
  floatRadix _ = 2
  floatDigits _ = fractionBits (sizes :: Sizes fs) + 1
  floatRange _ = let m = shiftL 1 (exponentBits (sizes :: Sizes fs) - 1) in (3 - m, m)
  decodeFloat f = if sign f 
   then let (s, ex) = decodeFloat (f { sign = False }) in (-s, ex)
   else
     let Sizes eb fb = sizes :: Sizes fs in
     case floatCase f of
       Zero -> (0, 0)
       Normal ex s -> (setBit s fb, ex - fb)
       Denormal fr -> let n = fb + 1 - bitWidth fr in (shiftL fr n, 2 - shiftL 1 (eb - 1) - n - fb)
       Infinity -> (0, 0)
       NaN sig pl -> (0, 0)
  encodeFloat 0 _ = SoftFloat False Zero 
  encodeFloat s ex | s < 0 = (encodeFloat (-s) ex) { sign = True }
                   | otherwise = SoftFloat False $ 
    let Sizes eb fb = sizes :: Sizes fs
        slide = fb + 1 - bitWidth s
        s' = clearBit (shift s slide) fb
        ex' = ex + fb - slide 
    in
      if ex' >= shiftL 1 (eb - 1) 
       then
         Infinity
       else
         let exMin = 1 - shiftL 1 (eb - 1) in
         if ex' > exMin
           then
             Normal ex' s'
           else
             if ex' + fb > exMin
                then
                  Denormal (shift s (slide + ex' - exMin - 1))
                else
                  Zero
  isNaN (SoftFloat _ (NaN _ _)) = True
  isNaN (SoftFloat _ _) = False
  isInfinite (SoftFloat _ Infinity) = True
  isInfinite (SoftFloat _ _) = False
