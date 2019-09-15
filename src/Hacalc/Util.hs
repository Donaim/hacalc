
module Hacalc.Util where

import Data.Char
import Data.Maybe (isJust)
import PatternT.All
import Hacalc.Types
import Hacalc.UtilExternal

historyLimitTreeSize :: (PatternElement a) => Int -> [(Tree a, b, c)] -> ([(Tree a, b, c)], [(Tree a, b, c)])
historyLimitTreeSize limit hist = span (isJust . treeSizeLim limit . fst3) hist

-- If size is less than `n' then (Just size) else Nothing. Lazy
treeSizeLim :: (PatternElement a) => Int -> Tree a -> Maybe Int
treeSizeLim n t = case t of
	Leaf {} -> if 1 < n then Just 1 else Nothing
	Branch xs -> loop (n - 1) xs
		where
		loop left childs = case childs of
			[] -> Just (n - left)
			(x : xs) -> case treeSizeLim left x of
				Nothing -> Nothing
				Just cn -> loop (left - cn) xs

showHistory :: (PatternElement a) => History a ctx -> [(String, String)]
showHistory = map f
	where f (t, traceElem, ctx) = (stringifyTree0 t, stringifyTraceElem traceElem)

stackBuiltinRules :: (Monad m, PatternElement a) => [PureSimplificationF a] -> Rulesets a -> [[SimplificationF a m ctx]]
stackBuiltinRules pures patterns = map (\ ps -> map Right3 pures ++ map Left3 ps) patterns
