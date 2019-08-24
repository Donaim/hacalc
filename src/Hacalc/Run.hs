{-# LANGUAGE DeriveDataTypeable #-}

module Hacalc.Run where

import Data.Either
import Data.Data

import PatternT.All
import Hacalc.Parser
import Hacalc.Types
import Hacalc.Builtins
import Hacalc.Util

pureRules :: [PureSimplificationF]
pureRules =
	[ ruleAdd "$add"
	, ruleMul "$mult"
	, ruleSub "$sub"
	, ruleDiv "$div"
	, rulePow "$pow"
	, ruleMod "$mod"
	, ruleEqual "$equal"
	, ruleIsNum "$num?"
	, ruleIsInt "int?"
	, ruleIsFrac "fraction?"
	, ruleIsFloat "float?"
	, ruleFloat "float"
	, ruleLess "lt?"
	, ruleLessOrEq "le?"
	]

delimitingSymbols :: [String]
delimitingSymbols = ["+", "-", "*", "/", "^"]

mixedRules :: (Monad m) => Rulesets -> [[SimplificationF m ctx]]
mixedRules patterns = map (\ ps -> map Right3 pureRules ++ map Left3 ps) patterns

data RunOptions = RunOptions
	{ parseDelimiters                    :: [String]
	, parseDelimiterPreserveQuotesQ      :: Bool
	, parseSplitByNumbersQ               :: Bool
	, parseEnableCommentsQ               :: Bool
	, displayConcatByNumbersQ            :: Bool
	, interpretStepLimit                 :: Maybe Int
	, interpretTreeSizeLimit             :: Maybe Int
	, interpretCondRecursionLimit        :: Maybe Int
	} deriving (Eq, Show, Read, Typeable, Data)

hacalcRun :: (Monad m) => RunOptions -> [[SimplificationF m ctx]] -> ctx -> String -> Either ParseError (m (Stdout ctx, Stdout ctx))
hacalcRun options rules ctx line = either
	Left
	(Right . withTokens)
	(tokenize delimited)
	where
	uncommented =
		if   parseEnableCommentsQ options
		then fst3 $ partitionString "//" line
		else line
	delimiterMode =
		if   parseDelimiterPreserveQuotesQ options
		then DelimiterPreserveQuotes
		else DelimiterIgnoreQuotes
	delimited =
		if   null $ parseDelimiters options
		then uncommented
		else delimitSymbols delimiterMode (parseDelimiters options) uncommented

	withTokens tokens = do
		result <- loop (makeTree (Group tokens)) rules
		return (applyLimits result)

	applyLimits hist = (sizes, dropedSizes)
		where
		(steps, dropedSteps) = maybe
			(hist, [])
			(\ lim -> splitAt lim hist)
			(interpretStepLimit options)
		(sizes, dropedSizes) = maybe
			(steps, [])
			(\ lim -> historyLimitTreeSize lim hist)
			(interpretTreeSizeLimit options)

	loop tree [] = return []
	loop tree (ruleset : rest) = do
		history <- mixedApplySimplificationsUntil0Debug (interpretCondRecursionLimit options) ruleset ctx tree
		let newtree = if null history
			then tree
			else fst3 (last history)
		next <- loop newtree rest
		return (history ++ next)

interpretOneTree :: (Monad m) => ctx -> [[SimplificationF m ctx]] -> Tree -> m (Stdout ctx)
interpretOneTree ctx rules t = loop t rules
	where
	loop tree [] = return []
	loop tree (ruleset : rest) = do
		history <- mixedApplySimplificationsUntil0Debug (Just 8) ruleset ctx tree -- NOTE: limit is only 8, deep conditionals will be dropped
		let newtree = if null history
			then tree
			else fst3 (last history)
		next <- loop newtree rest
		return (history ++ next)

interpretOneTree0 :: (Monad m) => ctx -> Rulesets -> Tree -> m (Stdout ctx)
interpretOneTree0 ctx rules = interpretOneTree ctx (mixedRules rules)

interpretLine :: (Monad m) => ctx -> Rulesets -> String -> Either ParseError (m (Stdout ctx))
interpretLine ctx rules line = case tokenize (delimitSymbols DelimiterPreserveQuotes delimitingSymbols uncommented) of
	Left e -> Left e
	Right tokens -> Right $ interpretOneTree0 ctx rules (makeTree (Group tokens))
	where uncommented = fst3 $ partitionString "//" line

interpretTextWithRules :: (Monad m) => ctx -> Rulesets -> String -> [(String, Either ParseError (m (Stdout ctx)))]
interpretTextWithRules ctx rules text =
	text |> lines |> map (\ line -> (line, interpretLine ctx rules line))

interpretRulesAndText :: (Monad m) => ctx -> String -> String -> Either [ParseMatchError] [(String, Either ParseError (m (Stdout ctx)))]
interpretRulesAndText ctx rulesText exprText = do
	rules <- readPatterns rulesText
	return (interpretTextWithRules ctx rules exprText)

showHistory :: (Stdout ctx) -> [(String, String)]
showHistory = map f
	where
	f (t, traceElem, ctx) = (stringifyTree0 t, stringifyTraceElem traceElem)
