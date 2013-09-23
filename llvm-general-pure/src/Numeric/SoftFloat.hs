{-# LANGUAGE 
  TypeFamilies,
  ScopedTypeVariables
  #-}
module Numeric.SoftFloat where

import Prelude hiding (exponent)
import Control.Monad
import Control.Arrow

import Debug.Trace

import Data.Maybe
import Data.Bits
import Data.Word
import Data.Ratio
import Data.List
import Data.Ord
import Data.Monoid

import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable

sbits :: Integer -> String
sbits = reverse . intercalate " "
        . takeWhile (not . null) . map (take 4) . iterate (drop 4)
        . map (\i -> if odd i then '1' else '0')
        . takeWhile (/=0) . iterate (`div` 2)

--btrace s v = (\vv -> trace ("finished " ++ s ++ ": " ++ show vv) vv) $ trace ("starting " ++ s) v
--btraceB s v = (\vv -> trace ("finished " ++ s ++ ": " ++ sbits vv) vv) $ trace ("starting " ++ s) v
btrace s = id
btraceB s = id

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
  = Zero
  | Denormal { fraction :: Integer }
  | Normal { exponent :: Int, fraction :: Integer }
  | Infinity
  | NaN { quiet :: Bool, payload :: Integer }
  deriving (Eq, Ord, Read, Show)

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
    NaN qt pl | (qt || pl /= 0) && (pl >= 0 && shiftR pl (fb - 1) == 0) -> 
      return (ones, (if qt then bit (fb - 1) else 0) .|. pl)
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

roundUp :: RoundingMode -> Ordering -> Integer -> Bool
roundUp roundingMode ordering s =
  case (roundingMode, ordering) of
    (RoundDown, _) -> False
    (RoundTowardZero, _) -> False
    (RoundUp, _) -> True
    (_, LT) -> False
    (_, GT) -> True
    (RoundToNearestTiesAwayFromZero, EQ) -> True
    (RoundToNearestTiesToEven, EQ) -> odd s

encodeFloatRounding :: forall fs . HasSizes fs => RoundingMode -> Integer -> Int -> SoftFloat fs
encodeFloatRounding = encodeFloatRounding' 0

encodeFloatRounding' :: forall fs . HasSizes fs => Rational -> RoundingMode -> Integer -> Int -> SoftFloat fs
encodeFloatRounding' remainder roundingMode s ex
  | s == 0 = SoftFloat False Zero
  | s < 0 = (encodeFloatRounding' remainder (flipRounding roundingMode) (-s) ex) { sign = True }
  | otherwise = SoftFloat False $
  let Sizes eb fb = sizes :: Sizes fs
      exMin = 1 - shiftL 1 (eb - 1)
      bw = bitWidth s
      ex' = ex + bw - 1
      dropBits = btrace "dropBits" $ bw - fb - if ex' > exMin then 1 else (ex' - exMin)
      s' = shift s (-dropBits)
      droppedBits = btraceB "droppedBits" $ btraceB "s" s .&. (complement (shift (-1) dropBits))
      tiePoint = shift (btraceB "denom" $ denominator remainder) dropBits
      remainder' = 2*((denominator remainder)*droppedBits + (btraceB "numer" $ numerator remainder))
      (s'', ex'') = 
        if remainder' > 0 && roundUp roundingMode (compare remainder' tiePoint) s'
         then (s' + 1, ex' + if s'' == bit fb then 1 else 0)
         else (s', ex')
  in case () of
        -- these first case is a shortcut to avoid calcuating droppedBits when the
        -- input exponent is ridiculously small
    _ | ex' < -(bit eb) -> Zero
      | ex'' >= shiftL 1 (eb - 1) -> Infinity
      | s'' == 0 -> Zero
      | ex'' > exMin -> Normal ex'' (clearBit s'' fb)
      | ex'' + fb >= exMin -> Denormal s''
    _ -> Zero

divideRounding :: forall fs . HasSizes fs => RoundingMode -> SoftFloat fs -> SoftFloat fs -> SoftFloat fs
divideRounding roundingMode = go
  where
    sf@(SoftFloat _ (NaN _ _)) `go` _ = sf { floatCase = (floatCase sf) { quiet = True } }
    _ `go` sf@(SoftFloat _ (NaN _ _)) = sf { floatCase = (floatCase sf) { quiet = True } }
    SoftFloat _ Infinity `go` SoftFloat _ Infinity = SoftFloat True (NaN True 0)
    SoftFloat _ Zero `go` SoftFloat _ Zero = SoftFloat True (NaN True 0)
    sf0@(SoftFloat s0 _) `go` sf1@(SoftFloat s1 _) | (s0 || s1) = (abs sf0 `go` abs sf1) { sign = (s0 /= s1) }
    SoftFloat False Infinity `go` SoftFloat False _ = SoftFloat False Infinity
    SoftFloat False _ `go` SoftFloat False Zero = SoftFloat False Infinity
    SoftFloat False _ `go` SoftFloat False Infinity = SoftFloat False Zero
    sf0 `go` sf1 = 
      let (s0, ex0) = btrace "sf0" $ decodeFloat sf0
          (s1, ex1) = btrace "sf1" $ decodeFloat sf1
          Sizes eb fb = sizes :: Sizes fs
          d = btrace "d" $ fb + if (btraceB "s0" s0 < btraceB "s1" s1) then 1 else 0
          (s', rem) = btrace "divMod: " $ (shiftL s0 d) `divMod` s1
          ex' = ex0 - ex1 - d
      in
        encodeFloatRounding' (rem % s1) roundingMode s' ex'

instance HasSizes fs => Num (SoftFloat fs) where
  sf@(SoftFloat _ (NaN _ _)) + _ = sf { floatCase = (floatCase sf) { quiet = True } }
  sf0 + sf1@(SoftFloat _ (NaN _ _)) = sf1 + sf0
  SoftFloat s0 Infinity + SoftFloat s1 Infinity | s0 /= s1 = SoftFloat True (NaN True 0)
  sf@(SoftFloat _ Infinity) + _ = sf
  sf0 + sf1@(SoftFloat _ Infinity) = sf1 + sf0
  sf@(SoftFloat True Zero) + SoftFloat True Zero = sf
  sf0 + sf1 = 
    let (s0, ex0) = decodeFloat sf0
        (s1, ex1) = decodeFloat sf1
        exMin = min ex0 ex1
    in
      encodeFloat (shiftL s0 (ex0 - exMin) + shiftL s1 (ex1 - exMin)) exMin

  sf@(SoftFloat _ (NaN _ _)) - _ = sf { floatCase = (floatCase sf) { quiet = True } }
  _ - sf@(SoftFloat _ (NaN _ _)) = sf { floatCase = (floatCase sf) { quiet = True } }
  sf0 - sf1 = sf0 + (negate sf1)

  sf@(SoftFloat _ (NaN _ _)) * _ = sf { floatCase = (floatCase sf) { quiet = True } }
  sf0 * sf1@(SoftFloat _ (NaN _ _)) = sf1 + sf0
  SoftFloat _ Infinity * SoftFloat _ Zero = SoftFloat True (NaN True 0)
  SoftFloat s0 Infinity * SoftFloat s1 _ = SoftFloat (s0 /= s1) Infinity
  sf0 * sf1@(SoftFloat _ Infinity) = sf1 * sf0
  SoftFloat s0 Zero * SoftFloat s1 _ = SoftFloat (s0 /= s1) Zero
  sf0 * sf1@(SoftFloat _ Zero) = sf1 * sf0
  sf0 * sf1 = 
    let (s0, ex0) = decodeFloat sf0
        (s1, ex1) = decodeFloat sf1
    in
      encodeFloat (s0 * s1) (ex0 + ex1)

  negate s = s { sign = not (sign s) }
  fromInteger = flip encodeFloat 0
  abs s = s { sign = False }
  signum (SoftFloat _ Zero) = SoftFloat False Zero
  signum s = s { floatCase = Normal 0 0 } 

instance HasSizes fs => Eq (SoftFloat fs) where
  sf0 == sf1 = compare sf0 sf1 == EQ

instance HasSizes fs => Ord (SoftFloat fs) where
  compare (SoftFloat _ (NaN _ _)) _ = GT
  compare _ (SoftFloat _ (NaN _ _)) = GT
  compare (SoftFloat _ Zero) (SoftFloat _ Zero) = EQ
  compare (SoftFloat False _) (SoftFloat True _) = GT
  compare (SoftFloat True _) (SoftFloat False _) = LT
  compare (SoftFloat True fc0) (SoftFloat True fc1) = compare fc1 fc0
  compare (SoftFloat False fc0) (SoftFloat False fc1) = compare fc0 fc1
  SoftFloat _ (NaN _ _) < _ = False
  _ < SoftFloat _ (NaN _ _) = False
  sf0 < sf1 = compare sf0 sf1 == LT
  SoftFloat _ (NaN _ _) <= _ = False
  _ <= SoftFloat _ (NaN _ _) = False
  sf0 <= sf1 = compare sf0 sf1 /= GT
  SoftFloat _ (NaN _ _) > _ = False
  _ > SoftFloat _ (NaN _ _) = False
  sf0 > sf1 = compare sf0 sf1 == GT
  SoftFloat _ (NaN _ _) >= _ = False
  _ >= SoftFloat _ (NaN _ _) = False
  sf0 >= sf1 = compare sf0 sf1 /= LT

instance HasSizes fs => Real (SoftFloat fs) where
  toRational (SoftFloat _ (NaN _ _)) = 0 % 0
  toRational (SoftFloat False Infinity) = 1 % 0
  toRational (SoftFloat True Infinity) = 1 % 0
  toRational sf = let (s, ex) = decodeFloat sf in if ex >= 0 then shiftL s ex % 1 else s % bit (-ex)

instance HasSizes fs => Fractional (SoftFloat fs) where
  (/) = divideRounding RoundToNearestTiesToEven
  recip = (1/)
  fromRational r = (fromIntegral (numerator r)) / (fromIntegral (denominator r))
  
instance HasSizes fs => RealFrac (SoftFloat fs) where
  properFraction sf@(SoftFloat True _) = (negate *** negate) . properFraction . negate $ sf
  properFraction sf = 
    let (s, ex) = decodeFloat sf
    in (fromIntegral (shift s ex), encodeFloat (s .&. complement (shift (-1) (-ex))) ex)

instance HasSizes fs => Floating (SoftFloat fs) where

data RoundingMode
  = RoundToNearestTiesToEven
  | RoundToNearestTiesAwayFromZero
  | RoundUp
  | RoundDown
  | RoundTowardZero
  deriving (Eq, Read, Show)

flipRounding RoundDown = RoundUp
flipRounding RoundUp = RoundDown
flipRounding r = r

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
  encodeFloat = encodeFloatRounding RoundToNearestTiesToEven
  isNaN (SoftFloat _ (NaN _ _)) = True
  isNaN (SoftFloat _ _) = False
  isInfinite (SoftFloat _ Infinity) = True
  isInfinite (SoftFloat _ _) = False
  isDenormalized (SoftFloat _ (Denormal _)) = True
  isDenormalized (SoftFloat _ _) = False
  isNegativeZero (SoftFloat True Zero) = True
  isNegativeZero (SoftFloat _ _) = False
  isIEEE _ = True
