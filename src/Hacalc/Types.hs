
module Hacalc.Types where

import PatternT.Types
import Data.Coerce

newtype SimplifyMonad a = SimplifyMonad { unliftSimplifyMonad :: a } -- Identity monad
type SimplifyCtx = ()
type MonadicSimplifyT = MonadicSimplify SimplifyMonad SimplifyCtx
type SimlifyFT = SimplificationF SimplifyMonad SimplifyCtx

type Stdout = SimplifyMonad [(Tree, Either SimplifyPattern String, SimplifyCtx)]
type Rulesets = [[SimplifyPattern]]

type Number = Rational

instance Functor SimplifyMonad where
	fmap     = coerce
instance Applicative SimplifyMonad where
	pure     = SimplifyMonad
	(<*>)    = coerce
instance Monad SimplifyMonad where
	m >>= k = k (unliftSimplifyMonad m)
