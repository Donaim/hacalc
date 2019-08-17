
module Hacalc.Types where

import PatternT.Types

type Stdout ctx = [(Tree, Either SimplifyPattern String, ctx)]
type Rulesets = [[SimplifyPattern]]

data Number
	= NumberFrac Rational
	| NumberFloat Double
	| NumberNaN
	deriving (Eq, Show, Read)
