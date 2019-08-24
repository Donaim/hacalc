
module Hacalc.Util where

import Data.Coerce (coerce)
import Data.Char
import Data.Maybe (isJust)
import Text.Read (readMaybe)
import Data.Ratio (denominator, numerator)
import Numeric (showFFloat, readFloat)
import PatternT.All
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

toRationalPrecise :: Double -> Rational
toRationalPrecise x = positive |> readFloat |> head |> fst |> (*) sign
	where
	sign = if x < 0 then -1 else 1
	positive = if x < 0 then tail (show x) else (show x) -- for some reason, readFloat does not work with negatives

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

historyLimitTreeSize :: Int -> [(Tree, b, c)] -> ([(Tree, b, c)], [(Tree, b, c)])
historyLimitTreeSize limit hist = span (isJust . treeSizeLim limit . fst3) hist

-- If size is less than `n' then (Just size) else Nothing. Lazy
treeSizeLim :: Int -> Tree -> Maybe Int
treeSizeLim n t = case t of
	Leaf {} -> if 1 < n then Just 1 else Nothing
	Branch xs -> loop (n - 1) xs
		where
		loop left childs = case childs of
			[] -> Just (n - left)
			(x : xs) -> case treeSizeLim left x of
				Nothing -> Nothing
				Just cn -> loop (left - cn) xs

showHistory :: History ctx -> [(String, String)]
showHistory = map f
	where f (t, traceElem, ctx) = (stringifyTree0 t, stringifyTraceElem traceElem)

stackBuiltinRules :: (Monad m) => [PureSimplificationF] -> Rulesets -> [[SimplificationF m ctx]]
stackBuiltinRules pures patterns = map (\ ps -> map Right3 pures ++ map Left3 ps) patterns

newtype IdentityMonad a = IdentityMonad { unliftIdentityMonad :: a }

instance Functor IdentityMonad where
	fmap     = coerce
instance Applicative IdentityMonad where
	pure     = IdentityMonad
	(<*>)    = coerce
instance Monad IdentityMonad where
	m >>= k  = k (unliftIdentityMonad m)
