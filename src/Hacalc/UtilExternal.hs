
module Hacalc.UtilExternal where

import Data.Char
import Data.Coerce (coerce)
import Data.Maybe
import Text.Read (readMaybe)
import Data.Ratio (denominator, numerator)
import Numeric (showFFloat, readFloat)

(|>) :: a -> (a -> b) -> b
(|>) x f = f x
infixl 0 |>

readHFloat :: String -> Maybe Rational
readHFloat "" = Nothing
readHFloat s = readFloat s |> listToMaybe |> maybe Nothing (Just . ((*) sign) . fst)
	where
	sign = if head s == '-' then -1 else 1
	positive = if sign > 0 then s else tail s -- for some reason, readFloat does not work with negatives

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
