
module Hacalc.Types where

import PatternT.Types
import PatternT.Util

type History ctx = [(HTree, Either HSimplifyPattern String, ctx)]
type Stdout ctx = (String, History ctx, History ctx)
type Rulesets = [[HSimplifyPattern]]

data Number
	= NumberFrac Rational
	| NumberFloat Double
	| NumberNaN
	deriving (Eq, Show, Read)

type HLeafType = StringyLeaf
type HTree = Tree HLeafType
type HSimplifyPattern = SimplifyPattern HLeafType
type HSimplificationF m ctx = SimplificationF HLeafType m ctx
type HPureSimplificationF = PureSimplificationF HLeafType
