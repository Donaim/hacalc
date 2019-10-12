
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
	| NumberFrac Rational (Maybe Integer) -- ^ Just `base' or `fraction' display forms
	deriving (Show, Read)

instance PatternElement HLeafType where
	patternElemShow x = case x of
		HVar s -> s
		NumberNaN -> "NaN"
		NumberFrac x Nothing -> showFraction x
		NumberFrac x (Just b) -> showHFloat b 5 x

	patternElemRead s =
		if s == "NaN" || s == "Infinity"
		then NumberNaN
		else case readHRational s of
			Nothing -> HVar s
			Just (r, b) -> NumberFrac r b

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
