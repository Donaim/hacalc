
module Hacalc.Types where

import PatternT.Types
import PatternT.Util
import Hacalc.UtilExternal
import Data.Ratio (denominator, numerator)

type History ctx = [(HTree, Either HSimplifyPattern String, ctx)]
type Stdout ctx = (String, History ctx, History ctx)
type Rulesets = [[HSimplifyPattern]]

data Number
	= NumberNaN
	| NumberFrac Rational Bool -- ^ Bool True: be showed as fraction, False: showed as floating
	deriving (Show, Read)

data HLeafType
	= HLeafNum Number
	| HVar String
	deriving (Eq, Show, Read)

instance PatternElement HLeafType where
	patternElemShow x = case x of
		HVar s -> s
		HLeafNum x -> case x of
			NumberNaN -> "NaN"
			NumberFrac x sf ->
				if denominator x == 1
				then show $ numerator x
				else show x

	patternElemRead s =
		if s == "NaN" || s == "Infinity"
		then HLeafNum $ NumberNaN
		else case readHFrac s of
			Just x -> HLeafNum $ NumberFrac x True
			Nothing -> case readHFloat s of
				Just x -> HLeafNum $ NumberFrac x False
				Nothing -> HVar s

instance Eq Number where
	a == b = case a of
		NumberFrac x sf -> case b of
			NumberFrac y sf -> x == y
			NumberNaN {} -> False
		NumberNaN {} -> case b of
			NumberFrac {} -> False
			NumberNaN {} -> True

instance Ord Number where
	compare a b = case a of
		NumberFrac a sf -> case b of
			NumberFrac b sf -> compare a b
			NumberNaN {} -> LT
		NumberNaN {} -> case b of
			NumberFrac {} -> GT
			NumberNaN {} -> EQ

instance Ord HLeafType where
	compare a b = case a of
		HVar x -> case b of
			HVar y -> compare x y
			HLeafNum {} -> GT
		HLeafNum x -> case b of
			HLeafNum y -> compare x y
			HVar {} -> LT

type HTree = Tree HLeafType
type HSimplifyPattern = SimplifyPattern HLeafType
type HSimplificationF m ctx = SimplificationF HLeafType m ctx
type HPureSimplificationF = PureSimplificationF HLeafType
