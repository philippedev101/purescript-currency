module Data.Currency
  ( Money
  , money
  , mkMoney
  , fromCents
  , cents
  , fromNumber
  , toNumber
  , add
  , subtract
  , negate
  , abs
  , sign
  , compare
  , isZero
  , isPositive
  , isNegative
  , mulInt
  , mulRate
  , divide
  , pow
  , allocate
  , split
  , minimum
  , maximum
  , haveSameAmount
  , hasSubUnits
  , toFixed
  , toDecimal
  , toUnits
  , toJSON
  , fromJSON
  , convert
  , class CurrencyScale
  , scale
  , module Data.Currency.Rounding
  ) where

import Prelude hiding (add, negate, sub)
import Prelude as P

import Data.Array (mapWithIndex, replicate)
import Data.Foldable (sum)
import Data.Foldable as F
import Data.Int as Int
import Data.Maybe (Maybe)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.String.CodeUnits as SCU
import Data.Tuple (Tuple(..))
import JS.BigInt (BigInt)
import JS.BigInt as BI
import Type.Proxy (Proxy(..))

import Data.Currency.Rounding (RoundingMode(..), roundDiv)

-- | An amount of money stored as an integer count of minor currency units
-- | (e.g. cents for EUR/USD, yen for JPY).
-- |
-- | The phantom type parameter `c` tracks the currency at the type level,
-- | preventing accidental mixing of different currencies.
newtype Money :: Symbol -> Type
newtype Money c = Money BigInt

derive newtype instance eqMoney :: Eq (Money c)
derive newtype instance ordMoney :: Ord (Money c)

instance showMoney :: Show (Money c) where
  show (Money v) = "(Money " <> show v <> ")"

instance semiringMoney :: Semiring (Money c) where
  zero = Money (BI.fromInt 0)
  one = Money (BI.fromInt 1)
  add (Money a) (Money b) = Money (P.add a b)
  mul (Money a) (Money b) = Money (P.mul a b)

instance ringMoney :: Ring (Money c) where
  sub (Money a) (Money b) = Money (P.sub a b)

-- | Type class for currencies with a known minor-unit scale.
-- | The scale is the number of minor units per major unit
-- | (e.g. 100 for EUR/USD, 1 for JPY, 1000 for BHD).
class CurrencyScale :: Symbol -> Constraint
class CurrencyScale c where
  scale :: Proxy c -> Int

instance CurrencyScale "EUR" where scale _ = 100
instance CurrencyScale "USD" where scale _ = 100
instance CurrencyScale "GBP" where scale _ = 100
instance CurrencyScale "CHF" where scale _ = 100
instance CurrencyScale "JPY" where scale _ = 1
instance CurrencyScale "BHD" where scale _ = 1000
instance CurrencyScale "KWD" where scale _ = 1000

-- Helpers

bi0 :: BigInt
bi0 = BI.fromInt 0

bi1 :: BigInt
bi1 = BI.fromInt 1

biAbs :: BigInt -> BigInt
biAbs n = if n < bi0 then P.negate n else n

-- | Get the currency code string from the phantom type.
currencyCode :: forall c. IsSymbol c => Proxy c -> String
currencyCode = reflectSymbol

-- | Number of decimal places for a given scale (e.g. 100 -> 2, 1000 -> 3).
scaleToDecimals :: Int -> Int
scaleToDecimals = go 0
  where
    go acc 1 = acc
    go acc n
      | n <= 0 = 0
      | otherwise = go (acc + 1) (n / 10)

-- | Left-pad a string with a character to a target length.
padStart :: Int -> Char -> String -> String
padStart targetLen ch s =
  let len = SCU.length s
  in if len >= targetLen then s
     else SCU.fromCharArray (replicate (targetLen - len) ch) <> s

----------------------------------------------------------------------
-- Construction
----------------------------------------------------------------------

-- | Construct a Money value from major + minor units with an explicit scale.
-- | e.g. `money 10 50 100` for 10.50 in a 2-decimal currency.
money :: forall c. Int -> Int -> Int -> Money c
money major minor minorPerMajor =
  Money (BI.fromInt major * BI.fromInt minorPerMajor + BI.fromInt minor)

-- | Construct a Money value from major + minor units using the currency's
-- | known scale. e.g. `mkMoney 10 50 :: Money "EUR"` for EUR 10.50.
mkMoney :: forall c. CurrencyScale c => Int -> Int -> Money c
mkMoney major minor = money major minor (scale (Proxy :: Proxy c))

-- | Construct from a raw count of minor units.
fromCents :: forall c. BigInt -> Money c
fromCents = Money

-- | Extract the raw minor unit count.
cents :: forall c. Money c -> BigInt
cents (Money v) = v

-- | Convert from a Number (representing major units, e.g. 10.50)
-- | by multiplying by minorPerMajor and truncating.
-- | Returns Nothing if the Number is not finite.
fromNumber :: forall c. Int -> Number -> Maybe (Money c)
fromNumber minorPerMajor n = do
  let scaled = n * Int.toNumber minorPerMajor
  bi <- BI.fromNumber scaled
  pure (Money bi)

-- | Convert to a Number (major units). Potentially lossy for very large amounts.
toNumber :: forall c. Int -> Money c -> Number
toNumber minorPerMajor (Money v) =
  BI.toNumber v / Int.toNumber minorPerMajor

----------------------------------------------------------------------
-- Arithmetic
----------------------------------------------------------------------

-- | Add two money amounts.
add :: forall c. Money c -> Money c -> Money c
add (Money a) (Money b) = Money (P.add a b)

-- | Subtract: a - b.
subtract :: forall c. Money c -> Money c -> Money c
subtract (Money a) (Money b) = Money (P.sub a b)

-- | Negate an amount.
negate :: forall c. Money c -> Money c
negate (Money a) = Money (P.negate a)

-- | Absolute value.
abs :: forall c. Money c -> Money c
abs (Money a) = Money (biAbs a)

-- | Returns -1 for negative, 0 for zero, 1 for positive.
sign :: forall c. Money c -> Int
sign (Money a)
  | a < bi0 = P.negate 1
  | a > bi0 = 1
  | otherwise = 0

-- | Compare two money amounts, returning an Ordering.
compare :: forall c. Money c -> Money c -> Ordering
compare (Money a) (Money b) = P.compare a b

----------------------------------------------------------------------
-- Predicates
----------------------------------------------------------------------

-- | Check if an amount is zero.
isZero :: forall c. Money c -> Boolean
isZero (Money a) = a == bi0

-- | Check if an amount is strictly positive.
isPositive :: forall c. Money c -> Boolean
isPositive (Money a) = a > bi0

-- | Check if an amount is strictly negative.
isNegative :: forall c. Money c -> Boolean
isNegative (Money a) = a < bi0

-- | Check if the amount has fractional minor units at the given scale.
-- | e.g. `hasSubUnits 100 (fromCents 1050)` is true (10.50 has sub-units),
-- |       `hasSubUnits 100 (fromCents 1000)` is false (10.00 has none).
hasSubUnits :: forall c. Int -> Money c -> Boolean
hasSubUnits minorPerMajor (Money v) = mod v (BI.fromInt minorPerMajor) /= bi0

----------------------------------------------------------------------
-- Scaling
----------------------------------------------------------------------

-- | Multiply by an integer scalar.
mulInt :: forall c. Money c -> Int -> Money c
mulInt (Money a) n = Money (a * BI.fromInt n)

-- | Multiply by a rational rate expressed as numerator/denominator.
-- | e.g. 19% VAT = `mulRate RoundHalfEven m 19 100`
-- | This avoids floating point entirely.
mulRate :: forall c. RoundingMode -> Money c -> Int -> Int -> Money c
mulRate mode (Money a) num den =
  Money (roundDiv mode (a * BI.fromInt num) (BI.fromInt den))

-- | Divide a money amount by an integer with a rounding mode.
-- | Unlike `allocate`, this does not preserve the total — rounding may
-- | cause a small discrepancy. Use `allocate` or `split` when the total
-- | must be preserved exactly.
divide :: forall c. RoundingMode -> Money c -> Int -> Money c
divide mode (Money a) n =
  Money (roundDiv mode a (BI.fromInt n))

-- | Raise a money amount to an integer power.
-- | Useful for compound interest calculations.
pow :: forall c. Money c -> Int -> Money c
pow (Money a) n = Money (BI.pow a (BI.fromInt n))

----------------------------------------------------------------------
-- Allocation
----------------------------------------------------------------------

-- | Distribute a Money amount according to a list of ratios.
-- | The remainder (due to integer division) is distributed one unit at a
-- | time across the parts, starting from the first.
-- | The output array has the same length as the ratios array.
-- | All ratios must be non-negative; the total of ratios must be positive.
allocate :: forall c. Money c -> Array Int -> Array (Money c)
allocate (Money total) ratios =
  let
    biRatios = map BI.fromInt ratios
    ratioSum = sum biRatios

    isNeg = total < bi0
    absTotal = biAbs total

    shares = map (\r -> absTotal * r / ratioSum) biRatios
    distributed = sum shares
    remainder = absTotal - distributed

    result = mapWithIndex (\i s ->
      let extra = if BI.fromInt i < remainder then bi1 else bi0
      in if isNeg then Money (P.negate (s + extra))
         else Money (s + extra)
    ) shares
  in
    result

-- | Split a Money amount into N equal parts, distributing the remainder.
split :: forall c. Money c -> Int -> Array (Money c)
split m n = allocate m (replicate n 1)

----------------------------------------------------------------------
-- Comparison helpers
----------------------------------------------------------------------

-- | Find the minimum of an array of money amounts.
minimum :: forall c. Array (Money c) -> Maybe (Money c)
minimum = F.minimum

-- | Find the maximum of an array of money amounts.
maximum :: forall c. Array (Money c) -> Maybe (Money c)
maximum = F.maximum

-- | Compare amounts across different currencies (ignoring currency).
-- | Returns true if the underlying minor unit counts are equal.
haveSameAmount :: forall c1 c2. Money c1 -> Money c2 -> Boolean
haveSameAmount (Money a) (Money b) = a == b

----------------------------------------------------------------------
-- Formatting
----------------------------------------------------------------------

-- | Format a Money amount as a decimal string with a specified number
-- | of decimal places. e.g. `toFixed 2 (fromCents 1050)` = "10.50"
toFixed :: forall c. Int -> Money c -> String
toFixed decimals (Money v) =
  let
    isNeg = v < bi0
    absV = biAbs v
    divisor = BI.pow (BI.fromInt 10) (BI.fromInt decimals)
    major = absV / divisor
    minor = mod absV divisor
    prefix = if isNeg then "-" else ""
    majorStr = show major
    minorStr = padStart decimals '0' (show minor)
  in
    if decimals <= 0 then prefix <> majorStr
    else prefix <> majorStr <> "." <> minorStr

-- | Format a Money amount as a decimal string, using the currency's
-- | known scale to determine the number of decimal places.
-- | e.g. for EUR (scale 100): `toDecimal (fromCents 1050)` = "10.50"
-- | e.g. for JPY (scale 1):   `toDecimal (fromCents 500)`  = "500"
toDecimal :: forall c. CurrencyScale c => Money c -> String
toDecimal m = toFixed (scaleToDecimals (scale (Proxy :: Proxy c))) m

-- | Split a Money amount into major and minor unit parts.
-- | Uses Euclidean division, so the minor part is always non-negative.
-- | e.g. `toUnits 100 (fromCents 1050)` = Tuple 10 50
toUnits :: forall c. Int -> Money c -> Tuple BigInt BigInt
toUnits minorPerMajor (Money v) =
  let biScale = BI.fromInt minorPerMajor
      major = v / biScale
      minor = mod v biScale
  in Tuple major minor

----------------------------------------------------------------------
-- Serialization
----------------------------------------------------------------------

-- | Serialize a Money amount to a currency code and amount string pair.
-- | e.g. `toJSON (fromCents 1050 :: Money "EUR")` = Tuple "EUR" "1050"
toJSON :: forall c. IsSymbol c => Money c -> Tuple String String
toJSON (Money v) = Tuple (currencyCode (Proxy :: Proxy c)) (show v)

-- | Deserialize a Money amount from a currency code and amount string pair.
-- | Returns Nothing if the amount string is not a valid integer.
-- | Note: the currency code is not validated against the phantom type —
-- | the caller must ensure it matches.
fromJSON :: forall c. Tuple String String -> Maybe (Money c)
fromJSON (Tuple _ amountStr) = do
  bi <- BI.fromString amountStr
  pure (Money bi)

----------------------------------------------------------------------
-- Currency conversion
----------------------------------------------------------------------

-- | Convert a Money amount from one currency to another using an exchange
-- | rate expressed as numerator/denominator to avoid floating point.
-- | e.g. EUR to USD at rate 1.0835: `convert RoundHalfEven m 10835 10000`
-- | The caller is responsible for adjusting for different minor unit scales
-- | between source and target currencies.
convert :: forall from to. RoundingMode -> Money from -> Int -> Int -> Money to
convert mode (Money a) rateNum rateDen =
  Money (roundDiv mode (a * BI.fromInt rateNum) (BI.fromInt rateDen))
