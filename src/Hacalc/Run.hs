{-# LANGUAGE DeriveDataTypeable #-}

module Hacalc.Run where

import Data.Either
import Data.Data

import PatternT.All
import Hacalc.Parser
import Hacalc.Types
import Hacalc.Builtins
import Hacalc.Util
import Hacalc.UtilExternal

-----------------------
-- GENERAL INTERFACE --
-----------------------

data InterpretOptions = InterpretOptions
	{ textDelimiters                     :: [String]
	, textDelimiterPreserveQuotesQ       :: Bool
	, textEnableCommentsQ                :: Bool
	, tokenizeRespectQuotesQ             :: Bool
	, tokenizeSplitByNumbersQ            :: Bool
	, parseFixMissingBracketsQ           :: Bool
	, parseReportMissingEndquoteQ        :: Bool
	, parseReportEmptyBracketsQ          :: Bool
	, displayConcatByNumbersQ            :: Bool
	, interpretStepLimit                 :: Maybe Int
	, interpretTreeSizeLimit             :: Maybe Int
	, interpretCondRecursionLimit        :: Maybe Int
	} deriving (Eq, Show, Read, Typeable, Data)

hacalcParse :: (PatternElement a) => InterpretOptions -> String -> Either ParseError (Tree a)
hacalcParse options line = either
	Left
	(Right . makeTree . Group . splitmaybe)
	(parse parseOptions $ tokenize (tokenizeRespectQuotesQ options) delimited)
	where
	parseOptions = ParseOptions
		{ fixMissingBrackets      = parseFixMissingBracketsQ    options
		, reportMissingEndQuote   = parseReportMissingEndquoteQ options
		, reportEmptyBrackets     = parseReportEmptyBracketsQ   options
		}
	splitmaybe =
		if   tokenizeSplitByNumbersQ options
		then splitByNumbers
		else id
	uncommented =
		if   textEnableCommentsQ options
		then fst3 $ partitionString "//" line
		else line
	delimiterMode =
		if   textDelimiterPreserveQuotesQ options
		then DelimiterRespectQuotes
		else DelimiterIgnoreQuotes
	delimited =
		if   null $ textDelimiters options
		then uncommented
		else delimitSymbols delimiterMode (textDelimiters options) uncommented

hacalcRunTree :: (PatternElement a, Monad m) => InterpretOptions -> [[SimplificationF a m ctx]] -> ctx -> (Tree a) -> m (Stdout a ctx)
hacalcRunTree options rules ctx tree = do
	result <- loop tree rules
	return (getStdout result)
	where
	-- getStdout :: (PatternElement a) => [(Tree a, Either (SimplifyPattern a) String, ctx)] -> Stdout a ctx
	getStdout result = (lastS, hist, droped)
		where
		(hist, droped) = applyLimits result
		lastTree = if null hist then tree else fst3 (last hist)
		concated = if displayConcatByNumbersQ options then concatByNumbers lastTree else lastTree
		lastS = stringifyTree0 concated

	applyLimits :: (PatternElement a) => [(Tree a, Either (SimplifyPattern a) String, ctx)] -> ([(Tree a, Either (SimplifyPattern a) String, ctx)], [(Tree a, Either (SimplifyPattern a) String, ctx)])
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

	-- loop :: (PatternElement a) => Tree a -> [[SimplificationF a m ctx]] -> m [(Tree a, Either (SimplifyPattern a) String, ctx)]
	loop tree [] = return []
	loop tree (ruleset : rest) = do
		history <- mixedApplySimplificationsUntil0Debug (interpretCondRecursionLimit options) ruleset ctx tree
		let newtree = if null history
			then tree
			else fst3 (last history)
		next <- loop newtree rest
		return (history ++ next)

hacalcRun :: (PatternElement a, Monad m) => InterpretOptions -> [[SimplificationF a m ctx]] -> ctx -> String -> Either ParseError (m (Stdout a ctx))
hacalcRun options rules ctx line = either
	Left
	(Right . hacalcRunTree options rules ctx)
	(hacalcParse options line)

---------------------
-- HACALC SPECIFIC --
---------------------

interpretLine :: (Monad m) => InterpretOptions -> Rulesets HLeafType -> ctx -> String -> Either ParseError (m (Stdout HLeafType ctx))
interpretLine options rules ctx line = hacalcRun options (stackBuiltinRules hacalcPureRules rules) ctx line

interpretTextWithRules :: (Monad m) => InterpretOptions -> Rulesets HLeafType -> ctx -> String -> [(String, Either ParseError (m (Stdout HLeafType ctx)))]
interpretTextWithRules options rules ctx text = text |> lines |> map (\ line -> (line, interpretLine options rules ctx line))

interpretRulesAndText :: (Monad m) => InterpretOptions -> String -> ctx -> String -> Either [ParseMatchError] [(String, Either ParseError (m (Stdout HLeafType ctx)))]
interpretRulesAndText options rulesText ctx exprText = do
	rules <- readPatterns rulesText
	return (interpretTextWithRules options rules ctx exprText)

hacalcPureRules :: [HPureSimplificationF]
hacalcPureRules =
	[ ruleAdd        "$add"
	, ruleMul        "$mul"
	, ruleSub        "$sub"
	, ruleDiv        "$div"
	, rulePow        "$pow"
	, ruleMod        "$mod"
	, ruleEqual      "$equal?"
	, ruleEq         "$eq?"
	, ruleEqualDynLim"$eqn?"
	, ruleOr         "$or"
	, ruleIsNum      "$num?"
	, ruleIsNan      "$nan?"
	, ruleIsInt      "$int?"
	, ruleIsFrac     "$fraction?"
	, ruleIsFloat    "$float?"
	, ruleFloat      "$float"
	, ruleLess       "$lt?"
	, ruleLessOrEq   "$le?"
	, ruleAlpha      "$alpha"
	, ruleBeta       "$beta"
	]

hacalcDelimitingSymbols :: [String]
hacalcDelimitingSymbols = ["$", "+", "-", "*", "/", "^"]
