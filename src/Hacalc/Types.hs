
module Hacalc.Types where

import PatternT.Types

type History ctx = [(Tree, Either SimplifyPattern String, ctx)]
type Stdout ctx = (String, History ctx, History ctx)
type Rulesets = [[SimplifyPattern]]

data Number
	= NumberFrac Rational
	| NumberFloat Double
	| NumberNaN
	deriving (Eq, Show, Read)
