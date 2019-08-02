
module Hacalc.Types where

import PatternT.Types

type SimplifyMonad = IO
type SimplifyCtx = ()
type MonadicSimplifyT = MonadicSimplify SimplifyMonad SimplifyCtx
type SimlifyFT = SimplificationF SimplifyMonad SimplifyCtx
