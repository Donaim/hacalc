
module Hacalc.Parser where

import Data.Either
import Control.Monad
import Data.List

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
splitLines = lines

splitRulesets :: [String] -> [[String]]
splitRulesets lines = loop [] [] lines
	where
	loop buf cur [] = reverse ((reverse cur) : buf)
	loop buf cur (x : xs) =
		if "-----" `isPrefixOf` x
		then loop ((reverse cur) : buf) [] xs
		else loop buf (x : cur) xs

readPatterns :: String -> Either [ParseMatchError] [[SimplifyPattern]]
readPatterns text = do
	unless (null badReads) (Left badReads)
	return (snd partitioned)
	where
	allLines = splitLines text
	rulesets = splitRulesets allLines

	reads :: [Either [ParseMatchError] [SimplifyPattern]]
	reads = map readOneRuleset rulesets

	partitioned :: ([[ParseMatchError]], [[SimplifyPattern]])
	partitioned = partitionEithers reads

	badReads :: [ParseMatchError]
	badReads = concat $ fst partitioned

readOneRuleset :: [String] -> Either [ParseMatchError] [SimplifyPattern]
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
