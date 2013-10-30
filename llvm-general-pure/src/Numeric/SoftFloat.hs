{-# LANGUAGE 
  TypeFamilies,
  ScopedTypeVariables
  #-}
module Numeric.SoftFloat where

import Prelude hiding (exponent)
import Control.Monad
import Control.Arrow
import Control.Applicative

import Debug.Trace

import Data.Function
import Data.Bits
import Data.Word
import Data.Ratio
import Data.List
import Data.Maybe

import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable

sbits :: Integer -> String
sbits = reverse . intercalate " "
        . takeWhile (not . null) . map (take 4) . iterate (drop 4)
        . map (\i -> if odd i then '1' else '0')
        . takeWhile (/=0) . iterate (`div` 2)

btraceQ ss s v = 
  if True
   then ((\vv -> trace ("finished " ++ s ++ ": " ++ ss vv) vv) $ trace ("starting " ++ s) v)
   else v
btrace :: Show v => String -> v -> v
btrace = btraceQ show
btraceB :: String -> Integer -> Integer
btraceB = btraceQ sbits

condTrace c s v = if c then btrace s v else v

data Sizes fs = Sizes { exponentBits :: Int, fractionBits :: Int }
  deriving (Read, Show)

class HasSizes fs where
  sizes :: Sizes fs

type family HardFloat rm fs :: *

newtype FSHalf = FSHalf ()
newtype FSSingle = FSSingle ()
newtype FSDouble = FSDouble ()
newtype FSQuadruple = FSQuadruple ()

class RoundingMode rm where
  applyRoundingMode :: rm -> Bool -> Rational -> Integer

newtype RMDown = RMDown ()
instance RoundingMode RMDown where 
  applyRoundingMode _ False = floor
  applyRoundingMode _ True = ceiling

newtype RMUp = RMUp ()
instance RoundingMode RMUp where 
  applyRoundingMode _ False = ceiling
  applyRoundingMode _ True = floor

newtype RMTowardZero = RMTowardZero ()
instance RoundingMode RMTowardZero where
  applyRoundingMode _ _ = truncate

newtype RMNearestTiesAwayFromZero = RMNearestTiesAwayFromZero ()
instance RoundingMode RMNearestTiesAwayFromZero where
  applyRoundingMode rm s r | r < 0 = negate . applyRoundingMode rm s . negate $ r
  applyRoundingMode _ _ r = let (d, f) = properFraction r in if f < (1/2) then d else d + 1

newtype RMNearestTiesToEven = RMNearestTiesToEven ()
instance RoundingMode RMNearestTiesToEven where
  applyRoundingMode _ _ = round

type instance HardFloat RMNearestTiesToEven FSSingle = Float
type instance HardFloat RMNearestTiesToEven FSDouble = Double

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
      ones = bit eb - 1
  case fc of
    Normal ex fr | ex >= 1 - m && ex <= m && shiftR fr fb == 0 -> return (ex + m - 1, fr)
                 where m = bit (eb - 1)
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
      | shiftR ex eb == 0 && shiftR fr fb == 0 -> return $ Normal (ex - bit (eb - 1) + 1) fr
      | otherwise -> fail "malformed input making FloatCase"
        where ones = bit eb - 1

data SoftFloat rm fs = SoftFloat { sign :: Bool, floatCase :: FloatCase fs }
  deriving (Read, Show)

changeRoundingMode :: SoftFloat rm fs -> SoftFloat rm' fs
changeRoundingMode (SoftFloat s fc) = SoftFloat s fc

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

toBytes :: forall rm fs . HasSizes fs => SoftFloat rm fs -> [Word8]
toBytes sf@(SoftFloat sgn fc) = either error id $ do
  let sz@(Sizes eb fb) = sizes :: Sizes fs
  (ex, fr) <- floatCaseToEF fc
  return . integerToBytes ((eb + fb + 1) `div` 8)
    $ shiftL (shiftL (if sgn then 1 else 0) eb .|. fromIntegral ex) fb .|. fr

fromBytes :: forall rm fs . HasSizes fs => [Word8] -> SoftFloat rm fs
fromBytes bytes = either error id $ do
  let sz@(Sizes eb fb) = sizes :: Sizes fs
  let bits = integerFromBytes $ bytes
  when (shiftR bits (eb + fb + 1) /= 0) $ fail "too many bits making SoftFloat"
  fc <- efToFloatCase (
    fromIntegral (clearBit (shiftR bits fb) eb), 
    bits .&. (bit fb - 1)
   )
  return SoftFloat {
    sign = testBit bits (eb + fb),
    floatCase = fc
  }

search :: (Int -> Bool) -> Int
search p = up 0 0
  where up r b = if p r then down r (b-1) else up (setBit r b) (b+1)
        down r (-1) = r
        down r b = let r' = clearBit r b in down (if p r' then r' else r) (b-1)

bitWidth :: Integer -> Int
bitWidth s = search (\b -> shiftR s b == 0)

onePlusIntLog2 :: Rational -> Int
onePlusIntLog2 r = let x b = 2^^b > r in if r <= 1 then 1 - search (not . x . negate) else search x

instance HasSizes fs => Storable (SoftFloat rm fs) where
  sizeOf _ = let Sizes eb fb = sizes :: Sizes fs in (eb + fb + 1) `div` 8
  alignment _ = 1
  poke p sf = do
    reverseIfBigEndian <- reverseIfBigEndian
    pokeArray (castPtr p) (reverseIfBigEndian . toBytes $ sf)
  peek p = do
    reverseIfBigEndian <- reverseIfBigEndian
    liftM (fromBytes . reverseIfBigEndian) (peekArray (sizeOf (undefined :: SoftFloat rm fs)) (castPtr p))

fromRationalEx :: forall rm fs . (RoundingMode rm, HasSizes fs) => Rational -> Int -> SoftFloat rm fs
fromRationalEx s0 ex0
  | s0 == 0 = SoftFloat False Zero
  | otherwise = SoftFloat (s0 < 0) $ 
  let Sizes eb fb = sizes :: Sizes fs
      s0' = abs s0
      exMin = 2 - bit (eb - 1)
      (s1, ex1) = let k = onePlusIntLog2 s0' - 1 in (s0' / 2^^k, ex0 + k)
      preRound = fb - max 0 (exMin - ex1)
      (s2, ex2) = (
        applyRoundingMode (undefined :: rm) (s0 < 0) (s1 * 2^^preRound),
        ex1 + if s2 == bit (preRound + 1) then 1 else 0
       )
  in case () of
        -- these first cases are a shortcuts to avoid useless large work  when the
        -- input exponent is ridiculously large or small
    _ | ex1 < -(bit eb) -> Zero
      | ex2 >= bit (eb - 1) -> Infinity
      | s2 == 0 -> Zero
      | ex2 >= exMin -> Normal ex2 (s2 .&. (bit fb - 1))
      | ex2 + fb >= exMin -> Denormal s2
    _ -> Zero

quietNaN sf = sf { floatCase = (floatCase sf) { quiet = True } }

instance (RoundingMode rm, HasSizes fs) => Num (SoftFloat rm fs) where
  sf@(SoftFloat _ (NaN _ _)) + _ = quietNaN sf
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

  sf@(SoftFloat _ (NaN _ _)) - _ = quietNaN sf
  _ - sf@(SoftFloat _ (NaN _ _)) = quietNaN sf
  sf0 - sf1 = sf0 + negate sf1

  sf@(SoftFloat _ (NaN _ _)) * _ = quietNaN sf
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

instance HasSizes fs => Eq (SoftFloat rm fs) where
  sf0 == sf1 = compare sf0 sf1 == EQ

instance HasSizes fs => Ord (SoftFloat rm fs) where
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

instance (RoundingMode rm, HasSizes fs) => Real (SoftFloat rm fs) where
  toRational (SoftFloat _ (NaN _ _)) = 0 % 0
  toRational (SoftFloat False Infinity) = 1 % 0
  toRational (SoftFloat True Infinity) = 1 % 0
  toRational sf = let (s, ex) = decodeFloat sf in if ex >= 0 then shiftL s ex % 1 else s % bit (-ex)

instance (RoundingMode rm, HasSizes fs) => Fractional (SoftFloat rm fs) where
  sf@(SoftFloat _ (NaN _ _)) / _ = sf { floatCase = (floatCase sf) { quiet = True } }
  _ / sf@(SoftFloat _ (NaN _ _)) = sf { floatCase = (floatCase sf) { quiet = True } }
  SoftFloat _ Infinity / SoftFloat _ Infinity = SoftFloat True (NaN True 0)
  SoftFloat _ Zero / SoftFloat _ Zero = SoftFloat True (NaN True 0)
  sf0@(SoftFloat s0 _) / sf1@(SoftFloat s1 _) | (s0 || s1) = (abs sf0 / abs sf1) { sign = (s0 /= s1) }
  SoftFloat False Infinity / SoftFloat False _ = SoftFloat False Infinity
  SoftFloat False _ / SoftFloat False Zero = SoftFloat False Infinity
  SoftFloat False _ / SoftFloat False Infinity = SoftFloat False Zero
  sf0 / sf1 = let [(s0, ex0), (s1, ex1)] = map decodeFloat [sf0, sf1] in fromRationalEx (s0 % s1) (ex0 - ex1)

  recip = (1/)
  fromRational = flip fromRationalEx 0
  
instance (RoundingMode rm, HasSizes fs) => RealFrac (SoftFloat rm fs) where
  properFraction sf@(SoftFloat True _) = (negate *** negate) . properFraction . negate $ sf
  properFraction sf = 
    let (s, ex) = decodeFloat sf
    in (fromIntegral (shift s ex), encodeFloat (s .&. complement (shift (-1) (-ex))) ex)

newtype Range = Range { getRangeExtrema :: [Rational] }
  deriving (Read, Show)

mkRange :: [Rational] -> Range
mkRange rs = Range (nub [minimum rs, maximum rs])

sRange :: Range -> String
sRange (Range rs) = show [ fromRational r :: Float | r <- rs ]

(.:) = (.).(.)

compareRanges :: Range -> Range -> Maybe Ordering
compareRanges (Range rs0) (Range rs1) = if all (== c) cs then Just c else Nothing
  where c:cs = [ compare r0 r1 | r0 <- rs0, r1 <- rs1 ]

instance Num Range where
  (+) = mkRange .: liftA2 (+) `on` getRangeExtrema
  negate = mkRange . liftA negate . getRangeExtrema
  (*) = mkRange .: liftA2 (*) `on` getRangeExtrema
  abs = mkRange . liftA abs . getRangeExtrema
  signum = mkRange . liftA signum . getRangeExtrema
  fromInteger = mkRange . return . fromInteger

instance Fractional Range where
  (/) = mkRange .: liftA2 (/) `on` getRangeExtrema
  fromRational = mkRange . return . fromRational

data Stream a = Stream { streamHead :: a, streamTail :: Stream a }

instance Functor Stream where
  fmap f xs = xs >>= return . f

instance Applicative Stream where
  pure = return
  (<*>) = ap

instance Monad Stream where
  return a = Stream a (return a)
  Stream a as >>= f = Stream (streamHead (f a)) (as >>= streamTail . f)
      
fromList (a:as) = Stream a (fromList as)
toList (Stream a as) = a : toList as

type ImprovingSequence = Stream Range

instance Num ImprovingSequence where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  negate = liftA negate
  abs = liftA abs
  signum = liftA signum
  fromInteger = return . fromInteger

instance Fractional ImprovingSequence where
  (/) = liftA2 (/)
  fromRational = return . fromRational

taylor :: (Eq f, Show f, RealFloat f) => [Rational] -> [Rational] -> Range -> ImprovingSequence
taylor coeffs bounds x = expand 0 1 1 coeffs bounds
  where expand :: Range -- sum
               -> Range -- d == x^k / k!
               -> Integer -- k' = k + 1
               -> [Rational] -- taylor coeffs, starting w/ kth
               -> [Rational] -- remainder bounds, starting w/ kth
               -> ImprovingSequence
        expand _ _ 200 _ _ = error "kNext too far"
        expand s d k' (c:cs) (m:ms) =
          let s' = s + (fromRational c)*d in
          Stream (s' + d * mkRange [ m, -m ]) (expand s' (d*x/(fromIntegral k')) (k'+1) cs ms)

invert :: Range -> (Rational -> ImprovingSequence) -> Rational -> ImprovingSequence
invert r f x = go r
  where go r@(Range [b,t]) = Stream r $ do
          let m = (b+t)/2
          case head . catMaybes . toList . fmap (compareRanges (fromRational x)) $ f m of
            LT -> go $ Range [b, m]
            GT -> go $ Range [m, t]
            EQ -> return (fromRational m)

limitBy :: (Rational -> x) -> (x -> x -> Bool) -> ImprovingSequence -> x
limitBy fromRational sameAs = 
  head . head . dropWhile (not . converged) . toList . fmap (map fromRational . getRangeExtrema)
    where converged (x:xs) = all (sameAs x) xs

limit :: (Eq f, RealFloat f) => ImprovingSequence -> f
limit = limitBy fromRational (\x y -> x == y && isNegativeZero x == isNegativeZero y)

decodeFloatRational :: forall rm fs . (RoundingMode rm, HasSizes fs) => SoftFloat rm fs -> (Rational, Int)
decodeFloatRational = ((% bit k) ***  (+ k)) . decodeFloat
  where k = fractionBits (sizes :: Sizes fs) + 1

expImp :: forall rm fs . (RoundingMode rm, HasSizes fs) => SoftFloat rm fs -> ImprovingSequence
expImp sf | sf > 1 = (taylor (repeat 1) (repeat 2.72) (fromRational s))^(2^ex)
          where (s, ex) = decodeFloatRational sf
expImp sf = let x = toRational sf in taylor (repeat 1) (repeat (2.72^(max 0 (ceiling x)))) (fromRational x)

logImp :: Rational -> ImprovingSequence
logImp x = -2 * (taylor
                (zipWith (*) (cycle [0,1]) (1 : factorials))
                (map (/(1-w^2)) (1 : factorials))
                (fromRational w))
  where w = (1-x)/(1+x)

factorials = scanl (*) 1 [1..]

atanImp :: Rational -> ImprovingSequence
atanImp = taylor (zipWith (*) (cycle [0,1,0,-1]) (0 : factorials)) (1 : factorials) . fromRational

sinImp :: Rational -> ImprovingSequence
sinImp = taylor (cycle [0,1,0,-1]) (repeat 1) . fromRational

tauImp :: ImprovingSequence
tauImp = 8*(4*atanImp (1/5) - atanImp (1/239))

modTauImp :: Rational -> ImprovingSequence
modTauImp r = fromRational r - tauImp * (fromIntegral $ (limitBy truncate (==) (fromRational r / tauImp) :: Integer))

instance (RoundingMode rm, HasSizes fs) => Floating (SoftFloat rm fs) where
  pi = limit (tauImp / 2)

  exp sf@(SoftFloat _ (NaN _ _)) = quietNaN sf
  exp sf@(SoftFloat False Infinity) = sf
  exp (SoftFloat True Infinity) = SoftFloat False Zero
  exp sf | sf > encodeFloat 1 (eb - 1) = SoftFloat False Infinity
         where Sizes eb fb = sizes :: Sizes fs
  exp sf | sf < encodeFloat (2 - fromIntegral fb) 0 - encodeFloat 1 (eb - 1) = SoftFloat False Zero
         where Sizes eb fb = sizes :: Sizes fs
  exp sf = limit (expImp sf)

  log sf@(SoftFloat _ (NaN _ _)) = quietNaN sf
  log (SoftFloat True Zero) = SoftFloat True (NaN True 0)
  log sf | sf < 0 = SoftFloat True (NaN True 0)
  log (SoftFloat False Zero) = SoftFloat True Infinity
  log (SoftFloat False Infinity) = SoftFloat False Infinity
  log sf = limit (logImp s - fromIntegral ex * logImp (1/2))
    where (s, ex) = decodeFloatRational sf

  sin sf@(SoftFloat _ (NaN _ _)) = quietNaN sf
  sin (SoftFloat _ Infinity) = SoftFloat True (NaN True 0)
  sin sf@(SoftFloat _ Zero) = sf
  sin sf | abs sf > 4 = limit (taylor (cycle [0,1,0,-1]) (repeat 1) =<< modTauImp (toRational sf))
  sin sf = limit . sinImp . toRational $ sf

  asin sf@(SoftFloat _ (NaN _ _)) = quietNaN sf
  asin sf | abs sf > 1 = SoftFloat False (NaN True 0)
  asin sf@(SoftFloat _ Zero) = sf
  asin sf = limit . invert (Range [-2,2]) (\x -> if limitBy (abs x <=) (==) (tauImp/4) then sinImp x else fromRational x + fromRational (signum x) * (tauImp / 4 - 1)) . toRational $ sf

  cos sf@(SoftFloat _ (NaN _ _)) = quietNaN sf
  cos (SoftFloat _ Infinity) = SoftFloat True (NaN True 0)
  cos sf | abs sf > 4 = limit (taylor (cycle [1,0,-1,0]) (repeat 1) =<< modTauImp (toRational sf))
  cos sf = limit . taylor (cycle [1,0,-1,0]) (repeat 1) . fromRational . toRational $ sf

  acos sf@(SoftFloat _ (NaN _ _)) = quietNaN sf
  acos sf | sf > 1 = SoftFloat True (NaN True 0)
{-
  acos sf = negate . limit . invert (Range [-4,0]) (\x -> case () of
                                                            _ | x > 0 -> fromRational x
                                                            _ | limitBy (x <=) (==) (-tauImp/2) then cosImp x else fromRational x + fromRational (signum x) * (tauImp / 4 - 1)) . toRational . negate $ sf
-}

  sinh sf@(SoftFloat _ (NaN _ _)) = quietNaN sf
  sinh sf@(SoftFloat _ Infinity) = sf
  sinh sf@(SoftFloat _ Zero) = sf
  sinh sf | sf < 0 = negate . sinh . negate $ sf
  sinh sf | sf > encodeFloat (bit (exponentBits (sizes :: Sizes fs))) 0 = SoftFloat False Infinity
  sinh sf = limit $ (expImp sf - expImp (-sf))/2
    
  cosh sf@(SoftFloat _ (NaN _ _)) = (quietNaN sf) { sign = False }
  cosh sf | sf < 0 = cosh (-sf)
  cosh sf@(SoftFloat _ Infinity) = sf
  cosh sf | sf > encodeFloat (bit (exponentBits (sizes :: Sizes fs))) 0 = SoftFloat False Infinity
  cosh sf = limit $ (expImp sf + expImp (-sf))/2

instance (RoundingMode rm, HasSizes fs) => RealFloat (SoftFloat rm fs) where
  floatRadix _ = 2
  floatDigits _ = fractionBits (sizes :: Sizes fs) + 1
  floatRange _ = let m = bit (exponentBits (sizes :: Sizes fs) - 1) in (3 - m, m)
  decodeFloat f = if sign f 
   then let (s, ex) = decodeFloat (abs f) in (-s, ex)
   else
     let Sizes eb fb = sizes :: Sizes fs in
     case floatCase f of
       Zero -> (0, 0)
       Normal ex s -> (setBit s fb, ex - fb)
       Denormal fr -> let n = fb + 1 - bitWidth fr in (shiftL fr n, 2 - shiftL 1 (eb - 1) - n - fb)
       Infinity -> (0, 0)
       NaN sig pl -> (0, 0)
  encodeFloat = fromRationalEx . (% 1)
  isNaN (SoftFloat _ (NaN _ _)) = True
  isNaN (SoftFloat _ _) = False
  isInfinite (SoftFloat _ Infinity) = True
  isInfinite (SoftFloat _ _) = False
  isDenormalized (SoftFloat _ (Denormal _)) = True
  isDenormalized (SoftFloat _ _) = False
  isNegativeZero (SoftFloat True Zero) = True
  isNegativeZero (SoftFloat _ _) = False
  isIEEE _ = True
