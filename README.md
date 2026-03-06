# purescript-currency

Type-safe, arbitrary-precision money library for PureScript. Built on native JavaScript `BigInt` via `purescript-js-bigints` — no floating point, no precision loss, no upper limit.

Money amounts are stored as integer minor units (e.g. cents) with a phantom type parameter tracking the currency at compile time, preventing accidental mixing of different currencies.

## Installation

```
spago install currency
```

## Quick start

```purescript
import Data.Currency (Money, mkMoney, add, mulRate, allocate, toDecimal, RoundingMode(..))

-- Type-safe: Money "EUR" and Money "USD" cannot be mixed
price :: Money "EUR"
price = mkMoney 99 99

-- 19% VAT — no floats, pure integer math
vat :: Money "EUR"
vat = mulRate RoundHalfEven price 19 100

total :: Money "EUR"
total = add price vat

-- Split a bill three ways, remainder distributed fairly
parts :: Array (Money "EUR")
parts = allocate total [1, 1, 1]

-- Format for display
formatted :: String
formatted = toDecimal total  -- "118.99"
```

## Why not just use Number?

JavaScript's `Number` type is IEEE 754 double-precision float:

```
> 0.1 + 0.2
0.30000000000000004
```

For financial calculations this is unacceptable. This library stores all amounts as `BigInt` integer cents — exact arithmetic with no rounding errors and no upper limit.

## Feature comparison

| Feature | bigint-money | Dinero.js | purescript-currency |
|---|---|---|---|
| Add / Subtract | yes | yes | yes |
| Multiply (scalar) | yes | yes | yes (`mulInt`) |
| Multiply (rate / percentage) | yes | yes | yes (`mulRate`) |
| Divide with rounding | yes | — | yes |
| Power / exponent | yes | — | yes |
| Negate | yes | — | yes |
| Abs | yes | — | yes |
| Sign (-1 / 0 / 1) | yes | — | yes |
| Compare (Ordering) | yes | yes | yes |
| Eq / Ord operators | — | — | yes (type class) |
| isZero / isPositive / isNegative | — | yes | yes |
| minimum / maximum | — | yes | yes |
| hasSubUnits | — | yes | yes |
| haveSameAmount | — | yes | yes |
| Allocate (by ratios) | yes | yes | yes |
| Split (equal parts) | — | — | yes |
| toFixed | yes | — | yes |
| toDecimal | — | yes | yes |
| toUnits (major / minor) | — | yes | yes |
| toJSON / fromJSON | yes | — | yes |
| Currency conversion | runtime | yes | yes |
| Currency safety | runtime check | runtime check | **compile-time** (phantom type) |
| CurrencyScale class | — | — | yes |
| Rounding modes | 4 | 8 | 8 |
| Scale normalization | implicit | yes | n/a (fixed scale per currency) |
| Arbitrary precision | yes (BigInt) | yes (BigInt) | yes (BigInt) |

### Key advantages over JS libraries

- **Compile-time currency safety** — `add (eurAmount) (usdAmount)` is a type error, not a runtime exception
- **8 rounding modes** including banker's rounding (`RoundHalfEven`), matching Dinero.js
- **No floating point anywhere** — rates are expressed as integer numerator/denominator
- **Zero JS dependencies** — uses native `BigInt` via `purescript-js-bigints`

## API

### Construction

```purescript
-- From major + minor units with explicit scale
money :: forall c. Int -> Int -> Int -> Money c
money 10 50 100  -- 10.50 in a 2-decimal currency

-- From major + minor units using CurrencyScale
mkMoney :: forall c. CurrencyScale c => Int -> Int -> Money c
mkMoney 10 50 :: Money "EUR"  -- EUR 10.50

-- From raw minor units
fromCents :: forall c. BigInt -> Money c

-- From a Number (goes through float — use for interop only)
fromNumber :: forall c. Int -> Number -> Maybe (Money c)
```

### Arithmetic

```purescript
add      :: forall c. Money c -> Money c -> Money c
subtract :: forall c. Money c -> Money c -> Money c
negate   :: forall c. Money c -> Money c
abs      :: forall c. Money c -> Money c

-- Operators work too: price + tax, total - discount
```

### Scaling

```purescript
-- Multiply by integer
mulInt :: forall c. Money c -> Int -> Money c

-- Multiply by rational rate (no floats!)
-- 19% VAT: mulRate RoundHalfEven amount 19 100
mulRate :: forall c. RoundingMode -> Money c -> Int -> Int -> Money c

-- Divide with rounding (does NOT preserve total — use allocate for that)
divide :: forall c. RoundingMode -> Money c -> Int -> Money c

-- Exponentiation
pow :: forall c. Money c -> Int -> Money c
```

### Allocation

```purescript
-- Distribute by ratios with fair remainder distribution
-- allocate (eur 100 0) [1, 1, 1] = [EUR 33.34, EUR 33.33, EUR 33.33]
allocate :: forall c. Money c -> Array Int -> Array (Money c)

-- Equal split (shorthand for allocate with equal ratios)
split :: forall c. Money c -> Int -> Array (Money c)
```

### Comparison

```purescript
compare :: forall c. Money c -> Money c -> Ordering
sign    :: forall c. Money c -> Int  -- -1, 0, or 1

isZero     :: forall c. Money c -> Boolean
isPositive :: forall c. Money c -> Boolean
isNegative :: forall c. Money c -> Boolean

minimum :: forall c. Array (Money c) -> Maybe (Money c)
maximum :: forall c. Array (Money c) -> Maybe (Money c)

-- Compare amounts across different currencies (ignoring currency)
haveSameAmount :: forall c1 c2. Money c1 -> Money c2 -> Boolean

hasSubUnits :: forall c. Int -> Money c -> Boolean
```

### Formatting

```purescript
-- Format with explicit decimal places
toFixed :: forall c. Int -> Money c -> String
toFixed 2 (fromCents 1050)  -- "10.50"

-- Format using the currency's known scale
toDecimal :: forall c. CurrencyScale c => Money c -> String
toDecimal (mkMoney 10 50 :: Money "EUR")  -- "10.50"
toDecimal (mkMoney 500 0 :: Money "JPY")  -- "500"

-- Split into major/minor BigInt pair
toUnits :: forall c. Int -> Money c -> Tuple BigInt BigInt
```

### Serialization

```purescript
toJSON   :: forall c. IsSymbol c => Money c -> Tuple String String
fromJSON :: forall c. Tuple String String -> Maybe (Money c)
```

### Currency conversion

```purescript
-- Rate as numerator/denominator to avoid floats
-- EUR to USD at 1.0835: convert RoundHalfEven eurAmount 10835 10000
convert :: forall from to. RoundingMode -> Money from -> Int -> Int -> Money to
```

### Rounding modes

| Mode | Behavior | Example (3.5) |
|---|---|---|
| `RoundDown` | Toward negative infinity | 3 |
| `RoundUp` | Toward positive infinity | 4 |
| `RoundToZero` | Toward zero (truncate) | 3 |
| `RoundFromZero` | Away from zero | 4 |
| `RoundHalfEven` | Banker's rounding (half to nearest even) | 4 |
| `RoundHalfOdd` | Half to nearest odd | 3 |
| `RoundHalfUp` | Half away from zero | 4 |
| `RoundHalfDown` | Half toward zero | 3 |

### CurrencyScale

Built-in scales for common currencies:

| Currency | Scale | Decimals |
|---|---|---|
| EUR, USD, GBP, CHF | 100 | 2 |
| JPY | 1 | 0 |
| BHD, KWD | 1000 | 3 |

Add your own:

```purescript
instance CurrencyScale "SEK" where scale _ = 100
```

## License

Apache-2.0
