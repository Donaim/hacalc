
module Hacalc.Parser where

import Data.Either
import Data.Maybe
import Control.Monad
import Data.List
import Data.Char

import PatternT.All
import Hacalc.Types
import Hacalc.UtilExternal
import Hacalc.Parsing
import Hacalc.Util

import Debug.Trace

partitionString :: String -> String -> (String, String, String)
partitionString break s =
	if breakIndex < 0
	then (s, "", "")
	else (take breakIndex s, break, drop (length break) after)

	where
	(after, breakIndex) = afterBreak break 0 s

	afterBreak :: String -> Int -> String -> (String, Int)
	afterBreak break pos [] = ("", -1)
	afterBreak break pos str =
		if isPrefixOf break str
		then (str, pos)
		else afterBreak break (pos + 1) (tail str)

splitLines :: String -> [String]
splitLines = map (dropWhile isSpace). lines

splitRulesets :: [String] -> [[String]]
splitRulesets lines = loop [] [] lines
	where
	loop buf cur [] = reverse ((reverse cur) : buf)
	loop buf cur (x : xs) =
		if "-----" `isPrefixOf` x
		then loop ((reverse cur) : buf) [] xs
		else loop buf (x : cur) xs

readPatterns :: (PatternElement a) => String -> Either [ParseMatchError] [[SimplifyPattern a]]
readPatterns = readPatternsL . splitLines

readPatternsL :: (PatternElement a) => [String] -> Either [ParseMatchError] [[SimplifyPattern a]]
readPatternsL allLines = do
	unless (null badReads) (Left badReads)
	return (snd partitioned)
	where
	rulesets = splitRulesets allLines

	-- reads :: (PatternElement a) => [Either [ParseMatchError] [SimplifyPattern a]]
	reads = map readOneRuleset rulesets

	-- partitioned :: (PatternElement a) => ([[ParseMatchError]], [[SimplifyPattern a]])
	partitioned = partitionEithers reads

	-- badReads :: [ParseMatchError]
	badReads = concat $ fst partitioned

readOneRuleset :: (PatternElement a) => [String] -> Either [ParseMatchError] [SimplifyPattern a]
readOneRuleset lines = do
	unless (null badRules) (Left badRules)
	return okRules
	where
	uncommented = map (fst3 . partitionString "//") lines
	filtered    = filter (not . isWhiteSpace) uncommented
	mrules      = map parseMatch filtered
	partitioned = partitionEithers mrules
	okRules     = snd partitioned
	badRules    = fst partitioned

concatByNumbers :: (PatternElement a) => Tree a -> Tree a
concatByNumbers t = case t of
	Branch [Leaf x, Leaf maybeMult, Leaf y] ->
		if patternElemShow maybeMult == "*" && isDecimal (patternElemShow x) && not (isDecimal (patternElemShow y))
		then Leaf $ patternElemReadUq $ patternElemShow x ++ patternElemShow y
		else t
	Branch xs -> Branch (map concatByNumbers xs)
	other -> t

isDecimal :: String -> Bool
isDecimal x = if null x then False else loop True x
	where
	loop expectedDot s = case s of
		[] -> True
		(x : xs) ->
			if isNumber x || (expectedDot && x == '.')
			then loop (expectedDot || x /= '.') xs
			else False

splitByNumbers :: [Expr] -> [Expr]
splitByNumbers = concatMap splitLeafByNumber

splitLeafByNumber :: Expr -> [Expr]
splitLeafByNumber x = case x of
	(Group xs) -> [Group $ concatMap splitLeafByNumber xs]
	(Atom s qq) ->
		if (isJust qq)
		then [x]
		else case splitStringByNumber s of
			([], ys) -> [x]
			(xs, []) -> [x]
			(xs, ys) -> [Atom xs Nothing, Atom "*" Nothing, Atom ys Nothing]

-- TODO: make better than O(n*m)
splitStringByNumber :: String -> (String, String)
splitStringByNumber original = r
	where
	ps = reverse $ zip (inits original) (tails original)
	filtered = filter (\ (i, t) -> isJust (readHRational i)) ps
	r = maybe (original, []) id (listToMaybe filtered)

