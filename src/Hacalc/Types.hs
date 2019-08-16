
module Hacalc.Types where

import PatternT.Types

type Stdout ctx = [(Tree, Either SimplifyPattern String, ctx)]
type Rulesets = [[SimplifyPattern]]

type Number = Rational
