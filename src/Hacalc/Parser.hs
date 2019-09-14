
module Hacalc.Parser where

import Data.Either
import Control.Monad
import Data.List
import Data.Char

import PatternT.All
import Hacalc.Types
import Hacalc.Util

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

readPatterns :: String -> Either [ParseMatchError] [[HSimplifyPattern]]
readPatterns = readPatternsL . splitLines

readPatternsL :: [String] -> Either [ParseMatchError] [[HSimplifyPattern]]
readPatternsL allLines = do
	unless (null badReads) (Left badReads)
	return (snd partitioned)
	where
	rulesets = splitRulesets allLines

	reads :: [Either [ParseMatchError] [HSimplifyPattern]]
	reads = map readOneRuleset rulesets

	partitioned :: ([[ParseMatchError]], [[HSimplifyPattern]])
	partitioned = partitionEithers reads

	badReads :: [ParseMatchError]
	badReads = concat $ fst partitioned

readOneRuleset :: [String] -> Either [ParseMatchError] [HSimplifyPattern]
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

concatByNumbers :: HTree -> HTree
concatByNumbers t = case t of
	Branch [Leaf x, Leaf maybeMult, Leaf y] ->
		if patternElemShow maybeMult == "*" && isDecimal x && not (isDecimal y)
		then Leaf (patternElemRead $ patternElemShow x ++ patternElemShow y)
		else t
	Branch xs -> Branch (map concatByNumbers xs)
	other -> t

isDecimal :: HLeafType -> Bool
isDecimal orig = let x = patternElemShow orig in if null x then False else loop True x
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
	(Atom s) -> case splitStringByNumber s of
		([], ys) -> [Atom s]
		(xs, []) -> [Atom s]
		(xs, ys) -> [Atom xs, Atom "*", Atom ys]

splitStringByNumber :: String -> (String, String)
splitStringByNumber original = loop True [] original
	where
	loop expectedDot buf s = case s of
		[] -> (reverse buf, [])
		(x : xs) ->
			if isSpace x
			then (original, [])
			else if isNumber x || (expectedDot && x == '.')
			then loop (expectedDot || x /= '.') (x : buf) xs
			else (reverse buf, x : xs)
