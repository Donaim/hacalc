
module Hacalc.Types where

import PatternT.Types
import PatternT.Util
import Hacalc.UtilExternal
import Data.Ratio (denominator, numerator)

type History ctx = [(HTree, Either HSimplifyPattern String, ctx)]
type Stdout ctx = (String, History ctx, History ctx)
type Rulesets = [[HSimplifyPattern]]

data HLeafType
	= HVar String
	| NumberNaN
	| NumberFrac Rational Bool -- ^ Bool True: be showed as fraction, False: showed as floating
	deriving (Show, Read)

instance PatternElement HLeafType where
	patternElemShow x = case x of
		HVar s -> s
		NumberNaN -> "NaN"
		NumberFrac x sf ->
			if denominator x == 1
			then show $ numerator x
			else show x

	patternElemRead s =
		if s == "NaN" || s == "Infinity"
		then NumberNaN
		else case readHFrac s of
			Just x -> NumberFrac x True
			Nothing -> case readHFloat s of
				Just x -> NumberFrac x False
				Nothing -> HVar s

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
