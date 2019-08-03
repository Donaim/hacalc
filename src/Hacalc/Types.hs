
module Hacalc.Types where

import PatternT.Types

type SimplifyMonad = IO
type SimplifyCtx = ()
type MonadicSimplifyT = MonadicSimplify SimplifyMonad SimplifyCtx
type SimlifyFT = SimplificationF SimplifyMonad SimplifyCtx

type Stdout = SimplifyMonad [(Tree, Either SimplifyPattern String, SimplifyCtx)]
type Rulesets = [[SimplifyPattern]]
