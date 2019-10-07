
module Hacalc.UtilExternal where

import Data.Char
import Data.Coerce (coerce)
import Data.Maybe
import Text.Read (readMaybe)
import Data.Ratio (denominator, numerator, (%))
import GHC.Real (Ratio(..))
import Numeric (showFFloat, readFloat)

(|>) :: a -> (a -> b) -> b
(|>) x f = f x
infixl 0 |>

readHFloat :: String -> Maybe (Rational, Integer)
readHFloat "" = Nothing
readHFloat s = case posParse False 10 [] [] positive of
	Nothing -> Nothing
	Just (base, before, after) ->
		if any (>= base) before || any (>= base) after
		then Nothing
		else toRat base before after |> flip (,) base |> Just
	where
	toRat base before after = ((sign * integralPart) :% 1) + (numer :% denom)
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

		('#' : xs) ->
			case readMaybe xs :: Maybe Integer of
				Nothing -> Nothing
				Just newbase ->
					if newbase < 0 || newbase > 36
					then Nothing
					else Just (newbase, before, after)

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

showRational :: Int -> Int -> Rational -> String
showRational base n r = (if r < 0 then "-" else "") ++ h ++ "." ++ f
	where
	bb = toInteger base
	d = round ((abs r) * ((toInteger (base ^ n)) % 1))
	s = showIntegerB bb (d :: Integer)
	s' = replicate (n - length s + 1) '0' ++ s
	(h, f) = splitAt (length s' - n) s'

showIntegerB :: Integer -> Integer -> String
showIntegerB base 0 = "0"
showIntegerB base n = reverse $ loop n
	where
	loop 0 = ""
	loop n = show dig ++ loop next
		where
		dig = n `mod` base
		next = n `div` base

readHFrac :: String -> Maybe Rational
readHFrac = readMaybe

-- | Strip all trailing zeroes
showNoZeroes :: (RealFloat a) => a -> String
showNoZeroes x = if anydotq then striped else s
	where
		s = showFullPrecision x
		r = reverse s
		anydotq = any (== '.') s
		striped = reverse $ (dropWhile (== '.') . dropWhile (== '0')) r

toRationalPrecise :: Double -> Rational
toRationalPrecise x = positive |> readFloat |> head |> fst |> (*) sign
	where
	sign = if x < 0 then -1 else 1
	positive = if x < 0 then tail (show x) else (show x) -- for some reason, readFloat does not work with negatives

showFullPrecision :: (RealFloat a) => a -> String
showFullPrecision x = showFFloat Nothing x ""

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
