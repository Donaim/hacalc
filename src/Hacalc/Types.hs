
module Hacalc.Types where

import PatternT.Types
import Data.Coerce

newtype SimplifyMonad a = SimplifyMonad { unliftSimplifyMonad :: a } -- Identity monad

type Stdout ctx = [(Tree, Either SimplifyPattern String, ctx)]
type Rulesets = [[SimplifyPattern]]

type Number = Rational

instance Functor SimplifyMonad where
	fmap     = coerce
instance Applicative SimplifyMonad where
	pure     = SimplifyMonad
	(<*>)    = coerce
instance Monad SimplifyMonad where
	m >>= k = k (unliftSimplifyMonad m)
