module Test.Main where

import Prelude hiding (add)

import Data.Maybe (Maybe(..), fromJust)
import Partial.Unsafe (unsafePartial)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

import JS.BigInt as BI

import Data.Currency (Money, money, mkMoney, fromCents, cents, add, subtract,
  isZero, isPositive, isNegative, mulInt, mulRate, divide, pow,
  allocate, split, haveSameAmount, hasSubUnits,
  toFixed, toDecimal, toUnits, toJSON, fromJSON, convert,
  RoundingMode(..))
import Data.Currency as Currency

type EUR = "EUR"
type USD = "USD"
type JPY = "JPY"
type BHD = "BHD"

eur :: Int -> Int -> Money EUR
eur major minor = money major minor 100

eurC :: Int -> Money EUR
eurC = fromCents <<< BI.fromInt

usdC :: Int -> Money USD
usdC = fromCents <<< BI.fromInt

jpyC :: Int -> Money JPY
jpyC = fromCents <<< BI.fromInt

unsafeFromJust :: forall a. Maybe a -> a
unsafeFromJust = unsafePartial fromJust

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do

  describe "Data.Currency" do

    describe "construction" do
      it "money creates correct cent value" do
        cents (eur 10 50) `shouldEqual` BI.fromInt 1050

      it "money 0 0 is zero" do
        cents (eur 0 0) `shouldEqual` BI.fromInt 0

      it "fromCents roundtrips" do
        cents (fromCents (BI.fromInt 999) :: Money EUR) `shouldEqual` BI.fromInt 999

      it "mkMoney uses CurrencyScale" do
        cents (mkMoney 10 50 :: Money EUR) `shouldEqual` BI.fromInt 1050

      it "mkMoney works for JPY (scale 1)" do
        cents (mkMoney 500 0 :: Money JPY) `shouldEqual` BI.fromInt 500

    describe "currency safety" do
      it "can add same currencies" do
        cents (add (eur 10 0) (eur 5 0)) `shouldEqual` BI.fromInt 1500

      -- NOTE: the following would not compile (type error):
      -- add (eur 10 0) (jpyC 500)

    describe "arithmetic" do
      it "adds two amounts" do
        cents (add (eur 10 50) (eur 3 25)) `shouldEqual` BI.fromInt 1375

      it "subtracts two amounts" do
        cents (subtract (eur 10 0) (eur 3 25)) `shouldEqual` BI.fromInt 675

      it "negates an amount" do
        cents (Currency.negate (eur 5 0)) `shouldEqual` BI.fromInt (-500)

      it "uses Semiring add via (+)" do
        cents (eur 1 0 + eur 2 0) `shouldEqual` BI.fromInt 300

      it "uses Ring sub via (-)" do
        cents (eur 5 0 - eur 2 0) `shouldEqual` BI.fromInt 300

    describe "abs" do
      it "abs of positive is identity" do
        cents (Currency.abs (eur 5 0)) `shouldEqual` BI.fromInt 500

      it "abs of negative is positive" do
        cents (Currency.abs (Currency.negate (eur 5 0))) `shouldEqual` BI.fromInt 500

      it "abs of zero is zero" do
        cents (Currency.abs (eur 0 0)) `shouldEqual` BI.fromInt 0

    describe "sign" do
      it "positive returns 1" do
        Currency.sign (eur 5 0) `shouldEqual` 1

      it "negative returns -1" do
        Currency.sign (Currency.negate (eur 5 0)) `shouldEqual` (-1)

      it "zero returns 0" do
        Currency.sign (eur 0 0) `shouldEqual` 0

    describe "compare" do
      it "less than" do
        Currency.compare (eur 5 0) (eur 10 0) `shouldEqual` LT

      it "equal" do
        Currency.compare (eur 10 0) (eur 10 0) `shouldEqual` EQ

      it "greater than" do
        Currency.compare (eur 10 0) (eur 5 0) `shouldEqual` GT

    describe "predicates" do
      it "isZero" do
        isZero (eur 0 0) `shouldEqual` true
        isZero (eur 1 0) `shouldEqual` false

      it "isPositive" do
        isPositive (eur 1 0) `shouldEqual` true
        isPositive (eur 0 0) `shouldEqual` false

      it "isNegative" do
        isNegative (Currency.negate (eur 1 0)) `shouldEqual` true
        isNegative (eur 0 0) `shouldEqual` false

      it "hasSubUnits detects fractional cents" do
        hasSubUnits 100 (eurC 1050) `shouldEqual` true

      it "hasSubUnits false for whole amounts" do
        hasSubUnits 100 (eurC 1000) `shouldEqual` false

    describe "scalar multiplication" do
      it "mulInt scales correctly" do
        cents (mulInt (eur 10 0) 3) `shouldEqual` BI.fromInt 3000

      it "mulInt by zero gives zero" do
        cents (mulInt (eur 10 0) 0) `shouldEqual` BI.fromInt 0

    describe "rate multiplication (mulRate)" do
      it "19% VAT on 100.00" do
        cents (mulRate RoundHalfEven (eur 100 0) 19 100) `shouldEqual` BI.fromInt 1900

      it "19% VAT on 10.00" do
        cents (mulRate RoundHalfEven (eur 10 0) 19 100) `shouldEqual` BI.fromInt 190

      it "rounds half to even (banker's rounding)" do
        cents (mulRate RoundHalfEven (eurC 5) 50 100) `shouldEqual` BI.fromInt 2
        cents (mulRate RoundHalfEven (eurC 15) 50 100) `shouldEqual` BI.fromInt 8

      it "RoundHalfUp rounds 0.5 away from zero" do
        cents (mulRate RoundHalfUp (eurC 5) 50 100) `shouldEqual` BI.fromInt 3

      it "RoundHalfDown rounds 0.5 toward zero" do
        cents (mulRate RoundHalfDown (eurC 5) 50 100) `shouldEqual` BI.fromInt 2
        cents (mulRate RoundHalfDown (eurC 15) 50 100) `shouldEqual` BI.fromInt 7

      it "RoundHalfOdd rounds 0.5 to nearest odd" do
        cents (mulRate RoundHalfOdd (eurC 5) 50 100) `shouldEqual` BI.fromInt 3
        cents (mulRate RoundHalfOdd (eurC 15) 50 100) `shouldEqual` BI.fromInt 7

      it "RoundDown always floors" do
        cents (mulRate RoundDown (eurC 10) 33 100) `shouldEqual` BI.fromInt 3

      it "RoundUp always ceils" do
        cents (mulRate RoundUp (eurC 10) 33 100) `shouldEqual` BI.fromInt 4

      it "RoundToZero truncates" do
        cents (mulRate RoundToZero (Currency.negate (eurC 10)) 33 100) `shouldEqual` BI.fromInt (-3)

      it "RoundFromZero goes away from zero" do
        cents (mulRate RoundFromZero (Currency.negate (eurC 10)) 33 100) `shouldEqual` BI.fromInt (-4)

    describe "divide" do
      it "divides evenly" do
        cents (divide RoundHalfEven (eur 10 0) 2) `shouldEqual` BI.fromInt 500

      it "divides with rounding" do
        cents (divide RoundHalfEven (eur 10 0) 3) `shouldEqual` BI.fromInt 333

      it "divides with RoundUp" do
        cents (divide RoundUp (eur 10 0) 3) `shouldEqual` BI.fromInt 334

      it "divide does not preserve total (unlike allocate)" do
        let part = divide RoundHalfEven (eur 10 0) 3
        cents (mulInt part 3) `shouldEqual` BI.fromInt 999

    describe "pow" do
      it "squares an amount" do
        cents (pow (eurC 10) 2) `shouldEqual` BI.fromInt 100

      it "cubes an amount" do
        cents (pow (eurC 3) 3) `shouldEqual` BI.fromInt 27

      it "power of 0 is 1" do
        cents (pow (eurC 5) 0) `shouldEqual` BI.fromInt 1

    describe "allocate" do
      it "splits 100.00 into [50, 50]" do
        let parts = allocate (eur 100 0) [50, 50]
        map cents parts `shouldEqual` [BI.fromInt 5000, BI.fromInt 5000]

      it "splits 100.00 into [70, 20, 10]" do
        let parts = allocate (eur 100 0) [70, 20, 10]
        map cents parts `shouldEqual` [BI.fromInt 7000, BI.fromInt 2000, BI.fromInt 1000]

      it "distributes remainder for 100.00 / 3" do
        let parts = allocate (eur 100 0) [1, 1, 1]
        map cents parts `shouldEqual` [BI.fromInt 3334, BI.fromInt 3333, BI.fromInt 3333]

      it "distributes remainder for 1.00 / 3" do
        let parts = allocate (eur 1 0) [1, 1, 1]
        map cents parts `shouldEqual` [BI.fromInt 34, BI.fromInt 33, BI.fromInt 33]

      it "handles zero ratios" do
        let parts = allocate (eur 100 0) [0, 50, 50]
        map cents parts `shouldEqual` [BI.fromInt 0, BI.fromInt 5000, BI.fromInt 5000]

      it "handles negative amounts" do
        let parts = allocate (Currency.negate (eur 100 0)) [1, 1, 1]
        map cents parts `shouldEqual` [BI.fromInt (-3334), BI.fromInt (-3333), BI.fromInt (-3333)]

      it "remainder spreads across multiple parts" do
        let parts = allocate (eur 1 0) [1,1,1,1,1,1,1]
        map cents parts `shouldEqual`
          [ BI.fromInt 15, BI.fromInt 15
          , BI.fromInt 14, BI.fromInt 14, BI.fromInt 14, BI.fromInt 14, BI.fromInt 14
          ]

      it "preserves total exactly" do
        let parts = allocate (eur 100 0) [1, 1, 1]
        parts `shouldEqual` [eurC 3334, eurC 3333, eurC 3333]

    describe "split" do
      it "splits evenly" do
        let parts = split (eur 10 0) 2
        map cents parts `shouldEqual` [BI.fromInt 500, BI.fromInt 500]

      it "distributes remainder" do
        let parts = split (eur 10 0) 3
        map cents parts `shouldEqual` [BI.fromInt 334, BI.fromInt 333, BI.fromInt 333]

    describe "minimum / maximum" do
      it "finds minimum" do
        Currency.minimum [eur 10 0, eur 5 0, eur 20 0] `shouldEqual` Just (eur 5 0)

      it "finds maximum" do
        Currency.maximum [eur 10 0, eur 5 0, eur 20 0] `shouldEqual` Just (eur 20 0)

      it "returns Nothing for empty array" do
        (Currency.minimum ([] :: Array (Money EUR))) `shouldEqual` Nothing
        (Currency.maximum ([] :: Array (Money EUR))) `shouldEqual` Nothing

    describe "haveSameAmount" do
      it "same amount different currencies" do
        haveSameAmount (eurC 1000) (usdC 1000) `shouldEqual` true

      it "different amounts" do
        haveSameAmount (eurC 1000) (usdC 500) `shouldEqual` false

    describe "formatting" do
      it "toFixed 2 for standard amounts" do
        toFixed 2 (eurC 1050) `shouldEqual` "10.50"

      it "toFixed 2 for whole euros" do
        toFixed 2 (eurC 1000) `shouldEqual` "10.00"

      it "toFixed 2 for sub-euro amounts" do
        toFixed 2 (eurC 5) `shouldEqual` "0.05"

      it "toFixed 2 for zero" do
        toFixed 2 (eurC 0) `shouldEqual` "0.00"

      it "toFixed 2 for negative amounts" do
        toFixed 2 (Currency.negate (eurC 1050)) `shouldEqual` "-10.50"

      it "toFixed 0 for JPY" do
        toFixed 0 (jpyC 500) `shouldEqual` "500"

      it "toFixed 3 for BHD (3 decimals)" do
        toFixed 3 (fromCents (BI.fromInt 12345) :: Money BHD) `shouldEqual` "12.345"

      it "toDecimal uses CurrencyScale for EUR" do
        toDecimal (eurC 1050) `shouldEqual` "10.50"

      it "toDecimal uses CurrencyScale for JPY" do
        toDecimal (jpyC 500) `shouldEqual` "500"

      it "toDecimal uses CurrencyScale for BHD" do
        toDecimal (fromCents (BI.fromInt 12345) :: Money BHD) `shouldEqual` "12.345"

    describe "toUnits" do
      it "splits into major and minor" do
        toUnits 100 (eurC 1050) `shouldEqual` Tuple (BI.fromInt 10) (BI.fromInt 50)

      it "handles zero minor" do
        toUnits 100 (eurC 1000) `shouldEqual` Tuple (BI.fromInt 10) (BI.fromInt 0)

      it "handles sub-unit amounts" do
        toUnits 100 (eurC 5) `shouldEqual` Tuple (BI.fromInt 0) (BI.fromInt 5)

      it "handles negative amounts" do
        -- Euclidean division: -1050 / 100 = -11, remainder 50
        -- For display, use toFixed/toDecimal instead which handle sign correctly
        toUnits 100 (Currency.negate (eurC 1050)) `shouldEqual` Tuple (BI.fromInt (-11)) (BI.fromInt 50)

    describe "serialization" do
      it "toJSON produces currency and amount" do
        toJSON (eurC 1050) `shouldEqual` Tuple "EUR" "1050"

      it "toJSON for negative" do
        toJSON (Currency.negate (eurC 500)) `shouldEqual` Tuple "EUR" "-500"

      it "fromJSON roundtrips" do
        let json = toJSON (eurC 1050)
        (map cents (fromJSON json :: Maybe (Money EUR))) `shouldEqual` Just (BI.fromInt 1050)

      it "fromJSON returns Nothing for invalid string" do
        (fromJSON (Tuple "EUR" "not-a-number") :: Maybe (Money EUR)) `shouldEqual` Nothing

    describe "currency conversion" do
      it "EUR to USD at rate 1.0835 (10835/10000)" do
        -- EUR 100.00 = 10000 cents -> 10000 * 10835 / 10000 = 10835 USD cents
        let eurAmount = eur 100 0
            usdAmount = convert RoundHalfEven eurAmount 10835 10000 :: Money USD
        cents usdAmount `shouldEqual` BI.fromInt 10835

      it "EUR to JPY at rate 162 (whole number)" do
        -- EUR 100.00 = 10000 cents, but JPY has no decimals
        -- Need to account for scale difference: 10000 * 162 / 100 = 16200 yen
        let eurAmount = eur 100 0
            jpyAmount = convert RoundHalfEven eurAmount 162 100 :: Money JPY
        cents jpyAmount `shouldEqual` BI.fromInt 16200

      it "conversion with rounding" do
        -- EUR 1.00 = 100 cents -> 100 * 10835 / 10000 = 108.35 -> 108
        let eurAmount = eur 1 0
            usdAmount = convert RoundHalfEven eurAmount 10835 10000 :: Money USD
        cents usdAmount `shouldEqual` BI.fromInt 108

  describe "Data.Currency — edge cases" do

    describe "fromNumber / toNumber" do
      it "fromNumber converts 10.50 to 1050 cents" do
        map cents (Currency.fromNumber 100 10.50 :: Maybe (Money EUR)) `shouldEqual` Just (BI.fromInt 1050)

      it "fromNumber converts 0.0 to 0 cents" do
        map cents (Currency.fromNumber 100 0.0 :: Maybe (Money EUR)) `shouldEqual` Just (BI.fromInt 0)

      it "toNumber converts 1050 cents to 10.50" do
        Currency.toNumber 100 (eurC 1050) `shouldEqual` 10.5

      it "toNumber converts 0 cents to 0.0" do
        Currency.toNumber 100 (eurC 0) `shouldEqual` 0.0

    describe "show instance" do
      it "shows positive" do
        show (eurC 1050) `shouldEqual` "(Money 1050)"

      it "shows negative" do
        show (Currency.negate (eurC 500)) `shouldEqual` "(Money -500)"

      it "shows zero" do
        show (eurC 0) `shouldEqual` "(Money 0)"

    describe "Eq / Ord operators" do
      it "equality" do
        (eurC 100 == eurC 100) `shouldEqual` true
        (eurC 100 == eurC 200) `shouldEqual` false

      it "inequality" do
        (eurC 100 /= eurC 200) `shouldEqual` true

      it "less than" do
        (eurC 100 < eurC 200) `shouldEqual` true
        (eurC 200 < eurC 100) `shouldEqual` false

      it "greater than or equal" do
        (eurC 100 >= eurC 100) `shouldEqual` true
        (eurC 200 >= eurC 100) `shouldEqual` true

    describe "large values (beyond Int32)" do
      it "handles amounts over 21 million euros" do
        -- 21_000_000_00 cents = 2.1 billion cents, exceeds Int32 max (2,147,483,647)
        let large = fromCents (BI.fromString "2500000000" # unsafeFromJust) :: Money EUR
        toFixed 2 large `shouldEqual` "25000000.00"

      it "adds large values correctly" do
        let a = fromCents (BI.fromString "9999999999999" # unsafeFromJust) :: Money EUR
            b = fromCents (BI.fromString "1" # unsafeFromJust) :: Money EUR
        cents (add a b) `shouldEqual` (BI.fromString "10000000000000" # unsafeFromJust)

      it "toFixed formats large values" do
        let large = fromCents (BI.fromString "123456789012345" # unsafeFromJust) :: Money EUR
        toFixed 2 large `shouldEqual` "1234567890123.45"

    describe "mulInt edge cases" do
      it "mulInt with negative scalar" do
        cents (mulInt (eurC 100) (-3)) `shouldEqual` BI.fromInt (-300)

    describe "negative rounding (non-half)" do
      it "RoundDown with negative: -10 * 33/100 = -3.3 -> -4" do
        cents (mulRate RoundDown (Currency.negate (eurC 10)) 33 100) `shouldEqual` BI.fromInt (-4)

      it "RoundUp with negative: -10 * 33/100 = -3.3 -> -3" do
        cents (mulRate RoundUp (Currency.negate (eurC 10)) 33 100) `shouldEqual` BI.fromInt (-3)

    describe "dividing zero" do
      it "zero divided by anything is zero" do
        cents (divide RoundHalfEven (eurC 0) 7) `shouldEqual` BI.fromInt 0
        cents (divide RoundUp (eurC 0) 3) `shouldEqual` BI.fromInt 0

    describe "allocate edge cases" do
      it "allocate zero amount" do
        let parts = allocate (eurC 0) [1, 1, 1]
        map cents parts `shouldEqual` [BI.fromInt 0, BI.fromInt 0, BI.fromInt 0]

      it "allocate with single ratio" do
        let parts = allocate (eur 100 0) [1]
        map cents parts `shouldEqual` [BI.fromInt 10000]

      it "allocate with very uneven ratios [999, 1]" do
        -- 10000 * 999 / 1000 = 9990, 10000 * 1 / 1000 = 10
        let parts = allocate (eur 100 0) [999, 1]
        map cents parts `shouldEqual` [BI.fromInt 9990, BI.fromInt 10]

      it "split into 1 part" do
        let parts = split (eur 100 0) 1
        map cents parts `shouldEqual` [BI.fromInt 10000]

    describe "formatting edge cases" do
      it "toDecimal for negative EUR" do
        toDecimal (Currency.negate (eurC 1050)) `shouldEqual` "-10.50"

      it "toFixed 2 for single cent" do
        toFixed 2 (eurC 1) `shouldEqual` "0.01"

    describe "conversion edge cases" do
      it "convert negative amount" do
        let neg = Currency.negate (eur 100 0)
            usd = convert RoundHalfEven neg 10835 10000 :: Money USD
        cents usd `shouldEqual` BI.fromInt (-10835)

      it "convert zero" do
        let z = eur 0 0
            usd = convert RoundHalfEven z 10835 10000 :: Money USD
        cents usd `shouldEqual` BI.fromInt 0

    describe "serialization edge cases" do
      it "fromJSON with negative string" do
        (map cents (fromJSON (Tuple "EUR" "-500") :: Maybe (Money EUR))) `shouldEqual` Just (BI.fromInt (-500))

      it "fromJSON with very large string" do
        let result = fromJSON (Tuple "EUR" "99999999999999999") :: Maybe (Money EUR)
        (map cents result) `shouldEqual` (BI.fromString "99999999999999999")

  describe "Data.Currency.Rounding" do

    describe "roundDiv edge cases" do
      it "exact division has no rounding effect" do
        mulRate RoundDown (eurC 100) 1 10 `shouldEqual` eurC 10
        mulRate RoundUp (eurC 100) 1 10 `shouldEqual` eurC 10
        mulRate RoundHalfEven (eurC 100) 1 10 `shouldEqual` eurC 10
        mulRate RoundHalfOdd (eurC 100) 1 10 `shouldEqual` eurC 10
        mulRate RoundHalfDown (eurC 100) 1 10 `shouldEqual` eurC 10

    describe "all 8 rounding modes on the same case" do
      it "RoundDown: 3.5 -> 3" do
        divide RoundDown (eurC 7) 2 `shouldEqual` eurC 3
      it "RoundUp: 3.5 -> 4" do
        divide RoundUp (eurC 7) 2 `shouldEqual` eurC 4
      it "RoundToZero: 3.5 -> 3" do
        divide RoundToZero (eurC 7) 2 `shouldEqual` eurC 3
      it "RoundFromZero: 3.5 -> 4" do
        divide RoundFromZero (eurC 7) 2 `shouldEqual` eurC 4
      it "RoundHalfUp: 3.5 -> 4 (away from zero)" do
        divide RoundHalfUp (eurC 7) 2 `shouldEqual` eurC 4
      it "RoundHalfDown: 3.5 -> 3 (toward zero)" do
        divide RoundHalfDown (eurC 7) 2 `shouldEqual` eurC 3
      it "RoundHalfEven: 3.5 -> 4 (nearest even)" do
        divide RoundHalfEven (eurC 7) 2 `shouldEqual` eurC 4
      it "RoundHalfOdd: 3.5 -> 3 (nearest odd)" do
        divide RoundHalfOdd (eurC 7) 2 `shouldEqual` eurC 3

    describe "negative half cases" do
      it "RoundHalfUp: -3.5 -> -4 (away from zero)" do
        divide RoundHalfUp (Currency.negate (eurC 7)) 2 `shouldEqual` Currency.negate (eurC 4)
      it "RoundHalfDown: -3.5 -> -3 (toward zero)" do
        divide RoundHalfDown (Currency.negate (eurC 7)) 2 `shouldEqual` Currency.negate (eurC 3)
      it "RoundHalfEven: -3.5 -> -4 (nearest even)" do
        divide RoundHalfEven (Currency.negate (eurC 7)) 2 `shouldEqual` Currency.negate (eurC 4)

    describe "banker's rounding symmetry" do
      it "2.5 -> 2 (even)" do
        divide RoundHalfEven (eurC 5) 2 `shouldEqual` eurC 2
      it "3.5 -> 4 (even)" do
        divide RoundHalfEven (eurC 7) 2 `shouldEqual` eurC 4
      it "4.5 -> 4 (even)" do
        divide RoundHalfEven (eurC 9) 2 `shouldEqual` eurC 4
      it "5.5 -> 6 (even)" do
        divide RoundHalfEven (eurC 11) 2 `shouldEqual` eurC 6
