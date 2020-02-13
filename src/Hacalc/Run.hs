{-# LANGUAGE DeriveDataTypeable #-}

module Hacalc.Run where

import Data.Either
import Data.Data

import PatternT.All
import Hacalc.Types
import Hacalc.Builtins
import Hacalc.UtilExternal
import PatterntCommonFrontend.All

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
	, ruleLog        "$log"
	, ruleMod        "$mod"
	, ruleBitOr      "$bitor"
	, ruleBitAnd     "$bitand"
	, ruleBitXor     "$bitxor"
	, ruleBitNot     "$bitnot"
	, ruleBitShiftR  "$bitshiftr"
	, ruleBitShiftL  "$bitshiftl"
	, ruleBitRotateR "$bitrotater"
	, ruleBitRotateL "$bitrotatel"
	, ruleEqual      "$equal?"
	, ruleEq         "$eq?"
	, ruleEqualDynLim"$eqn?"
	, ruleOr         "$or"
	, ruleIsNum      "$num?"
	, ruleIsNan      "$nan?"
	, ruleIsInt      "$int?"
	, ruleIsFrac     "$fraction?"
	, ruleIsFloat    "$float?"
	, ruleIsRational "$rat?"
	, ruleIsReal     "$real?"
	, ruleFrac       "$fraction"
	, ruleFloat      "$float"
	, ruleFloor      "$floor"
	, ruleCeiling    "$ceiling"
	, ruleRound      "$round"
	, ruleApprox     "$approx"
	, ruleDigits     "$digits"
	, ruleSinus      "$sinus"
	, ruleCosinus    "$cosinus"
	, rulePi         "$pi"
	, ruleExp1       "$exp1"
	, ruleLess       "$lt?"
	, ruleLessOrEq   "$le?"
	, ruleAlpha      "$alpha"
	, ruleBeta       "$beta"
	]

hacalcDelimitingSymbols :: [String]
hacalcDelimitingSymbols = ["$", "+", "-", "*", "/", "^", "|", "&", ">>", "<<", "~"]
