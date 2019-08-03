
module Hacalc.Run where

import Data.Either

import PatternT.Types
import PatternT.SimplifyInterface
import PatternT.Parsing
import PatternT.Display
import Hacalc.Parser
import Hacalc.Types
import Hacalc.Builtins
import Hacalc.Util

simplifyCtxInitial :: SimplifyCtx
simplifyCtxInitial = ()

showCtx :: SimplifyCtx -> String
showCtx = show

pureRules :: [PureSimplificationF]
pureRules =
	[ ruleAdd "$add"
	, ruleMult "$mult"
	, ruleSub "$sub"
	, ruleDiv "$div"
	, rulePow "$pow"
	, ruleEqual "$equal"
	, ruleIsNum "$num?"
	, ruleLess "lt?"
	, ruleLessOrEq "le?"
	]

mixedRules :: Rulesets -> [[SimlifyFT]]
mixedRules patterns = map (\ ps -> map Tuple32 pureRules ++ map Tuple30 ps) patterns

interpretOneTree :: [[SimlifyFT]] -> Tree -> Stdout
interpretOneTree rules t = loop t rules
	where
	loop tree [] = return []
	loop tree (ruleset : rest) = do
		history <- mixedApplySimplificationsWithPureUntil0Debug ruleset simplifyCtxInitial tree
		let newtree = if null history
			then tree
			else fst3 (last history)
		next <- loop newtree rest
		return (history ++ next)

interpretOneTree0 :: Rulesets -> Tree -> Stdout
interpretOneTree0 rules = interpretOneTree (mixedRules rules)

interpretLine :: Rulesets -> String -> Either ParseError Stdout
interpretLine rules line = case tokenize uncommented of
	Left e -> Left e
	Right tokens -> Right $ interpretOneTree0 rules (makeTree (Group tokens))
	where uncommented = fst3 $ partitionString "//" line

interpretTextWithRules :: Rulesets -> String -> [(String, Either ParseError Stdout)]
interpretTextWithRules rules text =
	text |> lines |> map (\ line -> (line, interpretLine rules line))

interpretRulesAndText :: String -> String -> Either [ParseMatchError] [(String, Either ParseError Stdout)]
interpretRulesAndText rulesText exprText = do
	rules <- readPatterns rulesText
	return (interpretTextWithRules rules exprText)

showHistory :: [(Tree, Either SimplifyPattern String, SimplifyCtx)] -> [(String, String, String)]
showHistory = map f
	where
	f (t, traceElem, ctx) = (stringifyTree t, stringifyTraceElem traceElem, showCtx ctx)
