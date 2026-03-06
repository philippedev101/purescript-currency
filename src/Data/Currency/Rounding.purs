module Data.Currency.Rounding
  ( RoundingMode(..)
  , roundDiv
  ) where

import Prelude

import JS.BigInt (BigInt, fromInt)

-- | Rounding modes for operations that may produce fractional minor units.
data RoundingMode
  = RoundDown        -- ^ Toward negative infinity (floor)
  | RoundUp          -- ^ Toward positive infinity (ceiling)
  | RoundToZero      -- ^ Toward zero (truncate)
  | RoundFromZero    -- ^ Away from zero
  | RoundHalfEven    -- ^ Banker's rounding: half rounds to nearest even
  | RoundHalfOdd     -- ^ Half rounds to nearest odd
  | RoundHalfUp      -- ^ Half rounds away from zero
  | RoundHalfDown    -- ^ Half rounds toward zero

derive instance eqRoundingMode :: Eq RoundingMode
derive instance ordRoundingMode :: Ord RoundingMode

instance showRoundingMode :: Show RoundingMode where
  show RoundDown = "RoundDown"
  show RoundUp = "RoundUp"
  show RoundToZero = "RoundToZero"
  show RoundFromZero = "RoundFromZero"
  show RoundHalfEven = "RoundHalfEven"
  show RoundHalfOdd = "RoundHalfOdd"
  show RoundHalfUp = "RoundHalfUp"
  show RoundHalfDown = "RoundHalfDown"

bi0 :: BigInt
bi0 = fromInt 0

bi1 :: BigInt
bi1 = fromInt 1

bi2 :: BigInt
bi2 = fromInt 2

-- | Integer division with a specified rounding mode.
-- | Given a numerator and a divisor, returns the quotient
-- | rounded according to the mode.
roundDiv :: RoundingMode -> BigInt -> BigInt -> BigInt
roundDiv mode num divisor =
  let
    truncQuot = truncDiv num divisor
    truncRem = num - truncQuot * divisor
  in
    case mode of
      RoundDown ->
        if truncRem < bi0 then truncQuot - bi1
        else truncQuot

      RoundUp ->
        if truncRem > bi0 then truncQuot + bi1
        else truncQuot

      RoundToZero ->
        truncQuot

      RoundFromZero ->
        if truncRem /= bi0 then
          if num >= bi0 then truncQuot + bi1
          else truncQuot - bi1
        else truncQuot

      RoundHalfUp ->
        halfRound truncQuot truncRem num divisor AwayFromZero

      RoundHalfDown ->
        halfRound truncQuot truncRem num divisor TowardZero

      RoundHalfEven ->
        halfRound truncQuot truncRem num divisor ToEven

      RoundHalfOdd ->
        halfRound truncQuot truncRem num divisor ToOdd

-- | Strategy for resolving exact-half cases.
data HalfStrategy = AwayFromZero | TowardZero | ToEven | ToOdd

-- | Shared logic for all half-rounding modes.
-- | When |remainder| > half the divisor, rounds away from zero.
-- | When |remainder| < half, rounds toward zero (truncate).
-- | When exactly half, delegates to the strategy.
halfRound :: BigInt -> BigInt -> BigInt -> BigInt -> HalfStrategy -> BigInt
halfRound truncQuot truncRem num divisor strategy =
  let
    absRem = abs truncRem
    absDivisor = abs divisor
    doubled = absRem * bi2
  in
    if doubled > absDivisor then
      -- More than half: always round away from zero
      roundAwayFromZero truncQuot num
    else if doubled < absDivisor then
      -- Less than half: always truncate
      truncQuot
    else
      -- Exactly half: apply strategy
      case strategy of
        AwayFromZero -> roundAwayFromZero truncQuot num
        TowardZero -> truncQuot
        ToEven ->
          if mod (abs truncQuot) bi2 == bi0 then truncQuot
          else roundAwayFromZero truncQuot num
        ToOdd ->
          if mod (abs truncQuot) bi2 /= bi0 then truncQuot
          else roundAwayFromZero truncQuot num

roundAwayFromZero :: BigInt -> BigInt -> BigInt
roundAwayFromZero q num =
  if num >= bi0 then q + bi1 else q - bi1

-- | Truncated division (toward zero), unlike PureScript's default
-- | Euclidean (floored) division.
truncDiv :: BigInt -> BigInt -> BigInt
truncDiv a b =
  let
    q = a / b
    r = a - q * b
  in
    if r /= bi0 && (a < bi0) /= (b < bi0) then q + bi1
    else q

abs :: BigInt -> BigInt
abs n = if n < bi0 then negate n else n
