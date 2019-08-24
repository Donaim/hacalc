{-# LANGUAGE DeriveDataTypeable #-}

module Hacalc.Run where

import Data.Either
import Data.Data

import PatternT.All
import Hacalc.Parser
import Hacalc.Types
import Hacalc.Builtins
import Hacalc.Util

-----------------------
-- GENERAL INTERFACE --
-----------------------

data InterpretOptions = InterpretOptions
	{ parseDelimiters                    :: [String]
	, parseDelimiterPreserveQuotesQ      :: Bool
	, parseSplitByNumbersQ               :: Bool
	, parseEnableCommentsQ               :: Bool
	, displayConcatByNumbersQ            :: Bool
	, interpretStepLimit                 :: Maybe Int
	, interpretTreeSizeLimit             :: Maybe Int
	, interpretCondRecursionLimit        :: Maybe Int
	} deriving (Eq, Show, Read, Typeable, Data)

hacalcParse :: InterpretOptions -> String -> Either ParseError Tree
hacalcParse options line = either
	Left
	(Right . makeTree . Group)
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

hacalcRunTree :: (Monad m) => InterpretOptions -> [[SimplificationF m ctx]] -> ctx -> Tree -> m (Stdout ctx)
hacalcRunTree options rules ctx tree = do
	result <- loop tree rules
	return (getStdout result)
	where
	getStdout result = (lastS, hist, droped)
		where
		(hist, droped) = applyLimits result
		lastTree = if null hist then tree else fst3 (last hist)
		concated = if displayConcatByNumbersQ options then concatByNumbers lastTree else lastTree
		lastS = stringifyTree0 concated

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

hacalcRun :: (Monad m) => InterpretOptions -> [[SimplificationF m ctx]] -> ctx -> String -> Either ParseError (m (Stdout ctx))
hacalcRun options rules ctx line = either
	Left
	(Right . hacalcRunTree options rules ctx)
	(hacalcParse options line)

---------------------
-- HACALC SPECIFIC --
---------------------

interpretLine :: (Monad m) => InterpretOptions -> Rulesets -> ctx -> String -> Either ParseError (m (Stdout ctx))
interpretLine options rules ctx line = hacalcRun options (stackBuiltinRules hacalcPureRules rules) ctx line

interpretTextWithRules :: (Monad m) => InterpretOptions -> Rulesets -> ctx -> String -> [(String, Either ParseError (m (Stdout ctx)))]
interpretTextWithRules options rules ctx text = text |> lines |> map (\ line -> (line, interpretLine options rules ctx line))

interpretRulesAndText :: (Monad m) => InterpretOptions -> String -> ctx -> String -> Either [ParseMatchError] [(String, Either ParseError (m (Stdout ctx)))]
interpretRulesAndText options rulesText ctx exprText = do
	rules <- readPatterns rulesText
	return (interpretTextWithRules options rules ctx exprText)

hacalcPureRules :: [PureSimplificationF]
hacalcPureRules =
	[ ruleAdd        "$add"
	, ruleMul        "$mult"
	, ruleSub        "$sub"
	, ruleDiv        "$div"
	, rulePow        "$pow"
	, ruleMod        "$mod"
	, ruleEqual      "$equal"
	, ruleIsNum      "$num?"
	, ruleIsInt      "int?"
	, ruleIsFrac     "fraction?"
	, ruleIsFloat    "float?"
	, ruleFloat      "float"
	, ruleLess       "lt?"
	, ruleLessOrEq   "le?"
	]

hacalcDelimitingSymbols :: [String]
hacalcDelimitingSymbols = ["+", "-", "*", "/", "^"]
