
module Hacalc.Util where

import Data.Char
import Data.Maybe (isJust)
import PatternT.All
import Hacalc.Types
import Hacalc.UtilExternal
import Hacalc.Display

-- | Simpliest working instance of PatternElement
newtype StringyLeaf = MkStringyLeaf { unStringyLeaf :: String }
	deriving (Eq, Show, Read, Ord)

treeToExpr :: (PatternElement a) => Tree a -> Expr
treeToExpr t = case t of
	Leaf s -> Atom (patternElemShow s) Nothing
	Branch xs -> Group (map treeToExpr xs)

appendQuotes :: QuoteInfo -> String -> String
appendQuotes q s = case q of
	Nothing -> s
	Just (qq, closedQ) ->
		qq : (escapeChar qq s)  ++ (if closedQ then [qq] else [])

escapeChar :: Char -> String -> String
escapeChar q s = case s of
	[] -> []
	(x : xs) ->
		if q == x
		then '\\' : x : escapeChar q xs
		else x : escapeChar q xs

instance PatternElement StringyLeaf where
	patternElemRead s q = MkStringyLeaf (appendQuotes q s)
	patternElemShow = unStringyLeaf

-- | Read unquoted, used frequently when creating new Leafs
patternElemReadUq :: (PatternElement a) => String -> a
patternElemReadUq s = patternElemRead s Nothing

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
