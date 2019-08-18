
module Hacalc.Util where

import Data.Coerce (coerce)
import Data.Char
import Text.Read (readMaybe)
import Data.Ratio (denominator, numerator)
import Numeric (showFFloat)
import PatternT.Types
import Hacalc.Types

fst3 :: (a, b, c) -> a
fst3 (a, b, c) = a

padLeft :: Char -> Int -> String -> String
padLeft c n s = s ++ (replicate toappend c)
	where
	toappend = max (n - (length s)) 0

isWhiteSpace :: String -> Bool
isWhiteSpace str = all isSpace str

(|>) :: a -> (a -> b) -> b
(|>) x f = f x
infixl 0 |>

-- | Strip all trailing zeroes
showNoZeroes :: (RealFloat a) => a -> String
showNoZeroes x = if anydotq then striped else s
	where
		s = showFullPrecision x
		r = reverse s
		anydotq = any (== '.') s
		striped = reverse $ (dropWhile (== '.') . dropWhile (== '0')) r

showFullPrecision :: (RealFloat a) => a -> String
showFullPrecision x = showFFloat Nothing x ""

numToTree :: Number -> Tree
numToTree x = case x of
	NumberNaN -> Leaf "NaN"
	NumberFloat x -> Leaf (showNoZeroes x)
	NumberFrac x ->
		if denominator x == 1
		then Leaf (show $ numerator x)
		else Leaf (show x)

symbolToMaybeNum :: Symbol -> Maybe Number
symbolToMaybeNum s =
	if s == "NaN" || s == "Infinity"
	then Just NumberNaN
	else case readMaybe s :: Maybe Rational of
		Just x -> Just (NumberFrac x)
		Nothing -> case readMaybe s :: Maybe Double of
			Just x -> Just (NumberFloat x)
			Nothing -> Nothing

treeToMaybeNum :: Tree -> Maybe Number
treeToMaybeNum t = case t of
	(Leaf s) -> symbolToMaybeNum s
	(Branch {}) -> Nothing

newtype IdentityMonad a = IdentityMonad { unliftIdentityMonad :: a }

instance Functor IdentityMonad where
	fmap     = coerce
instance Applicative IdentityMonad where
	pure     = IdentityMonad
	(<*>)    = coerce
instance Monad IdentityMonad where
	m >>= k  = k (unliftIdentityMonad m)
