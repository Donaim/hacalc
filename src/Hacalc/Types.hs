
module Hacalc.Types where

import PatternT.Types
import PatternT.Util
import Hacalc.UtilExternal
import Data.Ratio (denominator, numerator)
import Data.List (break)

type History a ctx = [(Tree a, Either (SimplifyPattern a) String, ctx)]
type Stdout a ctx = (String, History a ctx, History a ctx)
type Rulesets a = [[SimplifyPattern a]]

data HLeafType
	= HVar String
	| NumberNaN
	| NumberFrac Rational Bool -- ^ Bool True: be showed as fraction, False: showed as floating
	deriving (Show, Read)

instance PatternElement HLeafType where
	patternElemShow x = case x of
		HVar s -> s
		NumberNaN -> "NaN"
		NumberFrac x True ->
			if denominator x == 1
			then show $ numerator x
			else show (numerator x) ++ ('/' : show (denominator x))
		NumberFrac x False ->
			showHFloat 10 5 x

	patternElemRead s =
		if s == "NaN" || s == "Infinity"
		then NumberNaN
		else case break (== '/') s of
			([], ys) -> HVar s
			(xs, []) -> case readHFloat s of
				Just (r, b) -> NumberFrac r False
				Nothing -> HVar s
			(xs, ys) -> case r of
				Just (w, wb) -> NumberFrac w True
				Nothing -> HVar s
				where
				r = do
					(n, nb) <- readHFloat nums
					(d, db) <- readHFloat dens
					Just ((n / d), nb)
				nums = xs
				dens = tail ys

instance Eq HLeafType where
	a == b = case a of
		HVar x -> case b of
			HVar y -> x == y
			other -> False
		NumberFrac x sf -> case b of
			NumberFrac y sf -> x == y
			other -> False
		NumberNaN {} -> case b of
			NumberNaN {} -> True
			other -> False

instance Ord HLeafType where
	compare a b = case a of
		NumberNaN {} -> case b of
			NumberNaN {} -> EQ
			other -> GT
		HVar x -> case b of
			HVar y -> compare x y
			NumberNaN {} -> LT
			other -> GT
		NumberFrac x sf -> case b of
			NumberFrac y sf -> compare x y
			other -> LT

type HTree = Tree HLeafType
type HSimplifyPattern = SimplifyPattern HLeafType
type HSimplificationF m ctx = SimplificationF HLeafType m ctx
type HPureSimplificationF = PureSimplificationF HLeafType
