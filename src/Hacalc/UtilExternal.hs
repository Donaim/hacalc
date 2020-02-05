
module Hacalc.UtilExternal where

import Data.Char
import Data.Coerce (coerce)
import Data.Maybe
import Text.Read (readMaybe)
import Data.Ratio (denominator, numerator, (%))
import Numeric (showFFloat, readFloat)
import Data.Number.IReal
import Data.Number.IReal.IRealOperations
import Data.Number.IReal.IntegerInterval
import Data.Number.IReal.IReal

(|>) :: a -> (a -> b) -> b
(|>) x f = f x
infixl 0 |>

readHRational :: String -> Maybe (Rational, Maybe Integer)
readHRational s = case break (== '/') s of
	([], ys) -> Nothing
	(xs, []) -> case readHFloat s of
		Nothing -> Nothing
		Just (r, b) -> Just (r, (Just b))
	(xs, ys) -> case r of
		Nothing -> Nothing
		Just w -> Just (w, Nothing)
		where
		r = do
			(n, nb) <- readHFloat nums
			(d, db) <- readHFloat dens
			Just (n / d)
		nums = xs
		dens = tail ys

readHFloat :: String -> Maybe (Rational, Integer)
readHFloat "" = Nothing
readHFloat s = do
	(base, before, after) <- posParse False 10 [] [] positive
	if any (>= base) before || any (>= base) after || (null before && null after)
	then Nothing
	else Just (toRat base before after, base)
	where
	toRat base before after = ((sign * ((integralPart * denom) + numer)) % denom)
		where
		integralPart = parseIntegralPart base 1 before
		(numer, denom) = parseDecimalPart base 0 base after

	parseDecimalPart :: Integer -> Integer -> Integer -> [Integer] -> (Integer, Integer)
	parseDecimalPart base top down digits = case digits of
		[] -> (top, down)
		(x : xs) -> parseDecimalPart base (top + x * down) (base * down) xs

	parseIntegralPart :: Integer -> Integer -> [Integer] -> Integer
	parseIntegralPart base pow digits = case digits of
		[] -> 0
		(x : xs) -> x * pow + parseIntegralPart base (pow * base) xs

	posParse :: Bool -> Integer -> [Integer] -> [Integer] -> String -> Maybe (Integer, [Integer], [Integer])
	posParse doted base before after p = case p of
		[] -> Just (base, before, after)

		('.' : xs) ->
			if doted
			then Nothing
			else posParse True base before after xs

		('#' : xs) -> do
			newbase <- readBaseInteger xs
			Just (newbase, before, after)

		(x : xs) -> case charToDigit x of
			Nothing -> Nothing
			Just d ->
				if doted
				then posParse doted base before (d : after) xs
				else posParse doted base (d : before) after xs

	charToDigit :: Char -> Maybe Integer
	charToDigit c = case c of
		'0' -> Just 0
		'1' -> Just 1
		'2' -> Just 2
		'3' -> Just 3
		'4' -> Just 4
		'5' -> Just 5
		'6' -> Just 6
		'7' -> Just 7
		'8' -> Just 8
		'9' -> Just 9
		'a' -> Just 10
		'A' -> Just 10
		'b' -> Just 11
		'B' -> Just 11
		'c' -> Just 12
		'C' -> Just 12
		'd' -> Just 13
		'D' -> Just 13
		'e' -> Just 14
		'E' -> Just 14
		'f' -> Just 15
		'F' -> Just 15
		'g' -> Just 16
		'G' -> Just 16
		'h' -> Just 17
		'H' -> Just 17
		'i' -> Just 18
		'I' -> Just 18
		'j' -> Just 19
		'J' -> Just 19
		'k' -> Just 20
		'K' -> Just 20
		'l' -> Just 21
		'L' -> Just 21
		'm' -> Just 22
		'M' -> Just 22
		'n' -> Just 23
		'N' -> Just 23
		'o' -> Just 24
		'O' -> Just 24
		'p' -> Just 25
		'P' -> Just 25
		'q' -> Just 26
		'Q' -> Just 26
		'r' -> Just 27
		'R' -> Just 27
		's' -> Just 28
		'S' -> Just 28
		't' -> Just 29
		'T' -> Just 29
		'u' -> Just 30
		'U' -> Just 30
		'v' -> Just 31
		'V' -> Just 31
		'w' -> Just 32
		'W' -> Just 32
		'x' -> Just 33
		'X' -> Just 33
		'y' -> Just 34
		'Y' -> Just 34
		'z' -> Just 35
		'Z' -> Just 35
		oth -> Nothing

	sign = if head s == '-' then -1 else 1
	positive = if sign > 0 then s else tail s

showHFloat :: Integer -> Integer -> Rational -> String
showHFloat base n10 r = striped
	where
	n = if base == 10 then n10 else integerPrecisionBased10 base n10
	intn = fromInteger n
	d = round ((abs r) * ((base ^ n) % 1))
	sign = if r < 0 then "-" else ""
	x = showIntegerB base d
	repl = intn - (length x) + 1
	x' = replicate repl '0' ++ x
	(integralPart, decimalPart) = splitAt (length x' - intn) x'
	stripedDecimal = reverse $ dropWhile (== '0') $ reverse decimalPart
	suffix = if base == 10 then "" else '#' : show base
	striped = sign ++ integralPart ++ (if null stripedDecimal then "" else '.' : stripedDecimal) ++ suffix

integerPrecisionBased10 :: Integer -> Integer -> Integer
integerPrecisionBased10 base precision = loop 0 1
	where
	target = 10 ^ precision
	loop i b = if b < target then loop (i + 1) (b * base) else i

showIntegerB :: Integer -> Integer -> String
showIntegerB base 0 = "0"
showIntegerB base n = reverse $ loop n
	where
	loop 0 = ""
	loop n = intToChar dig : loop next
		where
		dig = n `mod` base
		next = n `div` base

	intToChar :: Integer -> Char
	intToChar x = case x of
		0 -> '0'
		1 -> '1'
		2 -> '2'
		3 -> '3'
		4 -> '4'
		5 -> '5'
		6 -> '6'
		7 -> '7'
		8 -> '8'
		9 -> '9'
		10 -> 'A'
		11 -> 'B'
		12 -> 'C'
		13 -> 'D'
		14 -> 'E'
		15 -> 'F'
		16 -> 'G'
		17 -> 'H'
		18 -> 'I'
		19 -> 'J'
		20 -> 'K'
		21 -> 'L'
		22 -> 'M'
		23 -> 'N'
		24 -> 'O'
		25 -> 'P'
		26 -> 'Q'
		27 -> 'R'
		28 -> 'S'
		29 -> 'T'
		30 -> 'U'
		31 -> 'V'
		32 -> 'W'
		33 -> 'X'
		34 -> 'Y'
		35 -> 'Z'
		ot -> '?' -- ASSUMPTION: max base is 36

showFraction :: Rational -> String
showFraction x =
	if denominator x == 1
	then show $ numerator x
	else show (numerator x) ++ ('/' : show (denominator x))

readRangedInteger :: Integer -> Integer -> String -> Maybe Integer
readRangedInteger min max s = do
	x <- readMaybe s
	if x < min || x > max
	then Nothing
	else Just x

maxBase :: Integer
minBase :: Integer
maxBase = 36
minBase = 2

isValidBase :: Integer -> Bool
isValidBase x = x <= maxBase && x >= minBase

readBaseInteger :: String -> Maybe Integer
readBaseInteger = readRangedInteger minBase maxBase

doubleIsNormal :: Double -> Bool
doubleIsNormal x = x == x && x /= doubleInfinity

doubleInfinity :: Double
doubleInfinity = 1 / 0

intMaxBoundInteger :: Integer
intMaxBoundInteger = toInteger (maxBound :: Int)

maybeIntegerToNonnegativeInt :: Integer -> Maybe Int
maybeIntegerToNonnegativeInt x =
	if x < 0 || x >= intMaxBoundInteger
	then Nothing
	else Just (fromInteger x)

iRealDefaultPrecision :: Integer
iRealDefaultPrecision = 2

iRealDefaultPrecisionI :: Int
iRealDefaultPrecisionI = fromIntegral iRealDefaultPrecision

iReal2RatP :: Int -> IReal -> Rational
iReal2RatP p x = case (readHRational $ showIReal p x) of -- TODO: improve performance-wise
	Just (r, b) -> r
	Nothing -> error "Impossible" -- ASSUMPTION: showIReal is always readable

iReal2Rat :: IReal -> Rational
iReal2Rat = iReal2RatP iRealDefaultPrecisionI

iRealCompareApprox :: IReal -> IReal -> Ordering
iRealCompareApprox x y =
	if lowerI dp >= 0 then GT
	else if upperI dp <= 0 then LT
	else EQ
	where dp = appr (x - y) iRealDefaultPrecisionI

fst3 :: (a, b, c) -> a
fst3 (a, b, c) = a

padLeft :: Char -> Int -> String -> String
padLeft c n s = s ++ (replicate toappend c)
	where
	toappend = max (n - (length s)) 0

isWhiteSpace :: String -> Bool
isWhiteSpace str = all isSpace str

newtype IdentityMonad a = IdentityMonad { unliftIdentityMonad :: a }

instance Functor IdentityMonad where
	fmap     = coerce
instance Applicative IdentityMonad where
	pure     = IdentityMonad
	(<*>)    = coerce
instance Monad IdentityMonad where
	m >>= k  = k (unliftIdentityMonad m)
