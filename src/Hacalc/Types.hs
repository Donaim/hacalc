{-# LANGUAGE TypeSynonymInstances #-}

module Hacalc.Types where

import PatternT.All
import PatterntCommonFrontend.All
import Hacalc.UtilExternal
import Data.Ratio (denominator, numerator)
import Data.List (break)
import Data.Number.IReal

instance Read MyIreal where
	readsPrec x str =
		case readHFloat str of
			Just (r, b) -> [(fromRational r, "")]
			Nothing -> []

type MyIreal = IReal

data HLeafType
	= HVar String
	| NumberNaN
	| NumberFrac Rational (Maybe Integer) -- ^ Just `base' or `fraction' display forms
	| NumberIrr MyIreal (Maybe Integer) -- ^ Just `base' or `fraction' display forms
	deriving (Show, Read)

instance PatternElement HLeafType where
	patternElemShow x = case x of
		HVar s -> s
		NumberNaN -> "NaN"
		NumberFrac x Nothing -> showFraction x
		NumberFrac x (Just b) -> showHFloat b 5 x
		NumberIrr x Nothing -> showFraction (iReal2Rat x)
		NumberIrr x (Just b) -> showHFloat b iRealDefaultPrecision (iReal2Rat x)

	patternElemRead s qq =
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
		NumberIrr x xb -> False -- NOTE: equality for reals is not decidable; TODO: make prettier

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
		NumberIrr x xb -> case b of
			HVar {} -> LT
			NumberNaN {} -> LT
			NumberFrac y yb -> compare (iReal2Rat x) y -- NOTE: not precise
			NumberIrr y yb -> iRealCompareApprox x y -- NOTE: not precise

type HTree = Tree HLeafType
type HSimplifyPattern = SimplifyPattern HLeafType
type HSimplificationF m ctx = SimplificationF HLeafType m ctx
type HPureSimplificationF = PureSimplificationF HLeafType
